<?php

declare(strict_types=1);

class HTML_Parser
{
  private int $_position;
  private int $_length;

  protected function __construct(public bool $strict = false, public array $selfClosingElements = [])
  {
  }

  /**
   * Analiza el documento HTML indicado, devolviendo su representación en un DOM
   */
  public static function parse(string $html, bool $strict = false, array $selfClosingElements = null): HTML_Parser_Document
  {
    $parser = new HTML_Parser($strict, $selfClosingElements ?? self::$defaultSelfClosingElements);
    return $parser->_parse($html);
  }

  private function _parse(string $html): HTML_Parser_Document
  {
    // Dividir en caracteres
    $this->_position = 0;
    $chars = preg_split('//u', $html, -1, PREG_SPLIT_NO_EMPTY);
    $this->_length = count($chars);

    // Procesar documento
    $document = new HTML_Parser_Document();
    $document->chars = $chars;
    $document->children = $this->_parseChunk($document, $document);

    return $document;
  }

  private function _parseChunk(HTML_Parser_Document $document, HTML_Parser_Document|HTML_Parser_Element $parentNode): array
  {
    /** @var array<HTML_Parser_Node> $nodes */
    $nodes = [];

    /** @var HTML_Parser_Text $currentTextNode */
    $currentTextNode = null;

    while ($this->_position < $this->_length) {
      $char = $document->chars[$this->_position];

      if ($char === '<'
          && ($nextChar = ($document->chars[$this->_position + 1] ?? ''))
          && (ctype_alpha($nextChar) || $nextChar == '/' || $this->_getSlice($document->chars, $this->_position, 4) == '<!--')) {
        if ($currentTextNode) {
          $currentTextNode->length = $this->_position - $currentTextNode->offset;
          $nodes[] = $currentTextNode;
          $currentTextNode = null;
        }

        if ($nextChar === '!' && $this->_getSlice($document->chars, $this->_position, 4) == '<!--') { // Comentario
          $nodes[] = $this->_parseComment($document->chars);
        } elseif ($nextChar === '/') { // Fin de elemento
          $tagStart = $this->_position;
          $closedTag = substr($this->_readUntil($document->chars, '>'), 2);
          $this->_position++;
          if ($parentNode instanceof HTML_Parser_Element) {
            $parentNode->end = $this->_position;

            if (trim($closedTag) != trim($parentNode->tag)) {
              $this->_error("Invalid closing tag, expected '{$parentNode->tag}' received '{$closedTag}'", $document);
            }

            $currentTextNode = null;
          } else { // Elemento inválido, marcarlo como texto
            $this->_error("Invalid closing found for unopened tag '{$closedTag}'", $document);

            $textNode = new HTML_Parser_Text();
            $textNode->offset = $tagStart;
            $textNode->length = $this->_position - $tagStart;
            $nodes[] = $textNode;
          }
          break;
        } else {
          $nodes[] = $this->_parseElement($document);
        }
      } else {
        if (!$currentTextNode) {
          $currentTextNode = new HTML_Parser_Text();
          $currentTextNode->offset = $this->_position;
        }
        $this->_position++;
      }
    }

    if ($currentTextNode) {
      $currentTextNode->length = $this->_position - $currentTextNode->offset;
      $nodes[] = $currentTextNode;
    }

    // Asignar padres
    foreach ($nodes as $node) {
      $node->parent = $parentNode;
      $node->document = $document;
    }

    return $nodes;
  }

  private function _parseElement(HTML_Parser_Document $document): HTML_Parser_Element
  {
    $element = new HTML_Parser_Element();
    $element->document = $document;

    // Leer nombre del elemento
    $element->offset = $this->_position;
    $this->_position++;
    $element->tag = $this->_readUntil($document->chars, fn($char) => ctype_space($char) || $char == '>' || $char == '/');

    // Leer atributos
    $this->_readWhitespaces($document->chars);
    $element->attributes = $this->_parseAttrs($document, $element);

    // Leer final de apertura del elemento
    if ($document->chars[$this->_position] != '>') {
      $this->_error("Expected '>' character after element attribute reading", $document);
    }

    $this->_position++;

    if ($document->chars[$this->_position - 2] == '/') { // Elemento autocerrado
      $element->autoClosed = true;
    } elseif (in_array(strtolower($element->tag), $this->selfClosingElements)) {
      $element->autoClosed = 'self';
    } elseif (in_array(strtolower($element->tag), ['script', 'style'])) { // Leer hasta el siguiente </tag>
      $cData = $this->_readCData($element);
      $cData->parent = $element;
      $cData->document = $document;
      $element->children = [$cData];
    } else { // Leer contenido
      $element->children = $this->_parseChunk($document, $element);
    }

    return $element;
  }

  private function _error(string $msg, HTML_Parser_Document $document): void
  {
    $line = array_count_values(array_slice($document->chars, 0, $this->_position))["\n"] ?? 0;
    $message = "{$msg} at position {$this->_position} line {$line}";

    if ($this->strict) {
      throw new Exception("{$message}: " . implode('', $document->chars));
    } else {
      error_log("{$message}: " . implode('', $document->chars));
    }
  }

  private function _parseAttrs(HTML_Parser_Document $document, HTML_Parser_Element $element): array
  {
    $attrs = [];

    while ($document->chars[$this->_position] != '>') { // Leer atributos
      $currentChar = $document->chars[$this->_position];

      if ($currentChar == '/' && ($document->chars[$this->_position + 1] ?? '') == '>') { // Elemento autocerrado />
        $this->_position++;
        break;
      }

      $this->_readWhitespaces($document->chars);

      $attr = new HTML_Parser_Attribute();
      $attr->offset = $this->_position;
      $attr->parent = $element;

      if ($currentChar == '"' || $currentChar == "'") {
        // Nombres de atributo entre comillas, por ejemplo: <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" 'http://www.w3.org/TR/html4/strict.dtd'>
        $attr->name = $this->_readAttrValue($document, $attr);
        $attr->name = $attr->enclosing . $attr->name . $attr->enclosing;
        $attr->enclosing = '';
      } else {
        $attr->name = $this->_readUntil($document->chars, fn($char) => $char == '=' || $char == '>' || $char == '/' || ctype_space($char));
      }

      $this->_readWhitespaces($document->chars);

      if ($document->chars[$this->_position] == '=') {
        $this->_position++;
        $this->_readWhitespaces($document->chars);

        $attr->value = $this->_readAttrValue($document, $attr);
      }

      $attrs[] = $attr;
    }

    return $attrs;
  }

  private function _readAttrValue(HTML_Parser_Document $document, HTML_Parser_Attribute $attr): string
  {
    // Los atributos pueden estar encerrados entre ", ' o sin comillas ni espacios
    if ($document->chars[$this->_position] == '"' || $document->chars[$this->_position] == "'") {
      $attr->enclosing = $document->chars[$this->_position];
      $this->_position++;
      $content = $this->_readUntil($document->chars, $attr->enclosing);
      $this->_position++; // Saltar final de atributo

      return $content;
    } else {
      $this->_readWhitespaces($document->chars);
      return $this->_readUntil($document->chars, fn($char) => ctype_space($char));
    }
  }


  private function _parseComment(array $html): HTML_Parser_Comment
  {
    $comment = new HTML_Parser_Comment();

    // Leer hasta -->
    $comment->offset = $this->_position;
    while ($this->_position < $this->_length) {
      if ($html[$this->_position] == '-' && $this->_getSlice($html, $this->_position, 3) == '-->') {
        $this->_position += 3;
        break;
      }
      $this->_position++;
    }

    $comment->value = $this->_getSlice($html, $comment->offset, $this->_position - $comment->offset);

    return $comment;
  }

  /**
   * @throws Exception
   */
  private function _readCData(HTML_Parser_Element $host): HTML_Parser_Text
  {
    $tagEnd = "</{$host->tag}>";
    $endFound = false;

    $content = new HTML_Parser_Text(
        $this->_readUntil(
            $host->document->chars,
            function ($char) use ($host, $tagEnd, &$endFound) {
              $endFound = $char == '<' && $this->_getSlice($host->document->chars, $this->_position, strlen($tagEnd)) == $tagEnd;
              return $endFound;
            }
        )
    );
    $content->offset = $this->_position;

    if (!$endFound) {
      $this->_error("Unclosed '{$host->tag}' element at position {$host->offset}", $host->document);
    } else {
      $this->_position += strlen($tagEnd); // Ajustar hasta la posición final del elemento
    }

    return $content;
  }

  private function _readUntil(array $html, string|callable $stopFn, bool $ignoreContent = false): string|null
  {
    $isString = is_string($stopFn);

    $start = $this->_position;
    while ($this->_position < $this->_length) {
      if ($isString
          ? $html[$this->_position] == $stopFn
          : call_user_func($stopFn, $html[$this->_position])) {
        break;
      }
      $this->_position++;
    }

    return $ignoreContent ? null : $this->_getSlice($html, $start, $this->_position - $start);
  }


  private function _readWhitespaces(array $html, bool $ignore = true): string|null
  {
    return $this->_readUntil($html, fn($char) => !ctype_space($char), $ignore);
  }

  private function _getSlice(array $html, int $offset, int $length = null): string
  {
    return implode('', array_slice($html, $offset, $length));
  }

  public static array $defaultSelfClosingElements = [
      '!doctype',
      'area',
      'base',
      'br',
      'col',
      'command',
      'embed',
      'hr',
      'img',
      'input',
      'keygen',
      'link',
      'menuitem',
      'meta',
      'param',
      'source',
      'track',
      'wbr'
  ];


  /**
   * Decodifica las entidades HTML de la cadena indicada
   */
  public static function entityDecode(string $html): string
  {
    return html_entity_decode($html, ENT_QUOTES, 'UTF-8');
  }

  /**
   * Codifica todas las entidades HTML de la cadena indicada
   */
  public static function entityEncode(string $html): string
  {
    return mb_convert_encoding($html, 'HTML-ENTITIES', 'UTF-8');
  }
}

trait HTML_Parser_ContainerNode
{
  /**
   * Recorre todos los nodos hijos del actual, llamando para cada uno de ellos a la función indicada
   *
   * @param callable(HTML_Parser_Node|HTML_Parser_Element|HTML_Parser_Comment|HTML_Parser_Text $node):void $callback
   */
  public function walk(callable $callback): int
  {
    return $this->_walkNodes($this->children, $callback);
  }

  private function _walkNodes(array $nodes, callable $callback): int
  {
    $count = 0;
    foreach ($nodes as $node) {
      call_user_func($callback, $node);
      $count++;

      if ($node instanceof HTML_Parser_Element) {
        $count += $this->_walkNodes($node->children, $callback);
      }
    }
    return $count;
  }

  /**
   * @return array<HTML_Parser_Element>
   */
  public function findAll(string $tag): array
  {
    $nodes = [];
    $this->walk(
        function ($node) use ($tag, &$nodes) {
          if ($node instanceof HTML_Parser_Element && strtolower($node->tag) == $tag) {
            $nodes[] = $node;
          }
        }
    );
    return $nodes;
  }
}

abstract class HTML_Parser_Node
{
  public HTML_Parser_Element|HTML_Parser_Document|null $parent;
  public HTML_Parser_Document|null $document;

  /**
   * Renders this node
   */
  public abstract function render(): string;

  public function remove(): void
  {
    $position = array_search($this, $this->parent->children);
    if ($position !== false) {
      unset($this->parent->children[$position]);
      $this->parent->children = array_values($this->parent->children);
      $this->parent = null;
      $this->document = null;
    }
  }

  /**
   * @param HTML_Parser_Node|array<HTML_Parser_Node> $node
   */
  public function replaceWith(HTML_Parser_Node|array $node): void
  {
    $position = array_search($this, $this->parent->children);
    if ($position !== false) {
      if (is_array($node)) {
        $this->parent->children = array_merge(
            array_slice($this->parent->children, 0, $position),
            $node,
            array_slice($this->parent->children, $position + 1)
        );
      } else {
        $this->parent->children[$position] = $node;
      }
    }
  }

  public function nextSibling(): HTML_Parser_Node|HTML_Parser_Comment|HTML_Parser_Element|HTML_Parser_Text|null
  {
    $position = array_search($this, $this->parent->children);
    if ($position !== false && count($this->parent->children) > $position + 1) {
      return $this->parent->children[$position + 1];
    }
    return null;
  }

  public function previousSibling(): HTML_Parser_Node|HTML_Parser_Comment|HTML_Parser_Element|HTML_Parser_Text|null
  {
    $position = array_search($this, $this->parent->children);
    if ($position !== false && $position - 1 >= 0) {
      return $this->parent->children[$position - 1];
    }
    return null;
  }
}

class HTML_Parser_Document
{
  use HTML_Parser_ContainerNode;

  /** @var array<HTML_Parser_Element|HTML_Parser_Comment|HTML_Parser_Text> */
  public array $children = [];

  /** @var array<string> */
  public array $chars;

  public function render(): string
  {
    $nodesHTML = [];
    foreach ($this->children as $node) {
      $nodesHTML[] = $node->render();
    }
    return implode('', $nodesHTML);
  }
}

class HTML_Parser_Element extends HTML_Parser_Node
{
  use HTML_Parser_ContainerNode;

  public string $tag;
  public int $offset;
  public int $end;
  public bool|string $autoClosed = false;

  /**
   * @var array<HTML_Parser_Attribute>
   */
  public array $attributes = [];

  /**
   * @var array<HTML_Parser_Node|HTML_Parser_Element|HTML_Parser_Comment|HTML_Parser_Text>
   */
  public array $children = [];

  /** @inheritDoc */
  public function render(): string
  {
    $html = "<{$this->tag}";

    if (!empty($this->attributes)) {
      $attributesHTML = [];
      foreach ($this->attributes as $attribute) {
        if ($attribute->value === null) {
          $attributesHTML[] = $attribute->name;
        } else {
          if ($attribute->enclosing && str_contains($attribute->value, $attribute->enclosing)) {
            $attribute->value = htmlspecialchars($attribute->value);
            $attribute->enclosing = '"';
          }
          $attributesHTML[] = "{$attribute->name}={$attribute->enclosing}{$attribute->value}{$attribute->enclosing}";
        }
      }
      $html .= ' ' . implode(' ', $attributesHTML);
    }


    if ($this->autoClosed) {
      $html .= ($this->autoClosed === 'self' ? '>' : '/>');
    } else {
      $html .= '>' . $this->innerHTML() . "</{$this->tag}>";
    }

    return $html;
  }

  public function innerHTML(): string
  {
    $childrenHTML = [];
    foreach ($this->children as $child) {
      $childrenHTML[] = $child->render();
    }
    return implode('', $childrenHTML);
  }

  public function innerText(): string
  {
    $childrenText = [];
    foreach ($this->children as $child) {
      if ($child instanceof HTML_Parser_Text) {
        $childrenText[] = $child->render();
      } elseif ($child instanceof HTML_Parser_Element) {
        $childrenText[] = $child->innerText();
      }
    }
    return implode('', $childrenText);
  }

  public function hasAttribute(string $name, bool $ignoreCase = true): HTML_Parser_Attribute|null
  {
    foreach ($this->attributes as $attr) {
      if ($ignoreCase ? strcasecmp($attr->name, $name) == 0 : $attr->name == $name) {
        return $attr;
      }
    }

    return null;
  }

  public function removeAttribute(string $name): bool
  {
    $attr = $this->hasAttribute($name);
    if ($attr) {
      $attr->remove();
      return true;
    } else {
      return false;
    }
  }

  public function setAttribute(string $name, string $value): HTML_Parser_Attribute
  {
    $alreadyExisting = $this->hasAttribute($name);

    $attr = $alreadyExisting ?: new HTML_Parser_Attribute();
    $attr->name = $name;
    $attr->value = $value;
    $attr->enclosing = '"';
    $attr->parent = $this;

    if (!$alreadyExisting) {
      $this->attributes[] = $attr;
    }
    return $attr;
  }

  public function getAttributeValue(string $name, string $default = null): string|null
  {
    $attr = $this->hasAttribute($name);
    return $attr->value ?? $default;
  }

  /**
   * @param HTML_Parser_Node|array<HTML_Parser_Node> $node
   *
   * @return $this
   */
  public function appendSibling(HTML_Parser_Node|array $node): self
  {
    $node = is_array($node) ? $node : [$node];
    $this->parent->children = $this->_arrayInsert($this->parent->children, array_search($this, $this->parent->children) + 1, $node);

    foreach ($node as $n) {
      $n->parent = $this->parent;
      $n->document = $this->document;
    }

    return $this;
  }

  private function _arrayInsert(array $array, int $offset, array $insert): array
  {
    return array_merge(array_slice($array, 0, $offset, false), $insert, array_slice($array, $offset));
  }

  /**
   * @param callable(HTML_Parser_Node $node): void $callback
   */
  public function walkParents(callable $callback): void
  {
    $parent = $this->parent;

    while ($parent) {
      call_user_func($callback, $parent);
      $parent = $parent->parent ?? null;
    }
  }

  /**
   * @param string|HTML_Parser_Node|array<HTML_Parser_Node> $content
   */
  public static function create(string $tag, array $attributes, string|HTML_Parser_Node|array $content = null): self
  {
    $element = new self();
    $element->tag = $tag;
    foreach ($attributes as $k => $v) {
      $attr = new HTML_Parser_Attribute();
      $attr->name = $k;
      $attr->enclosing = '"';
      $attr->value = $v;
      $element->attributes[] = $attr;
    }

    if (isset($content)) {
      if (is_string($content)) {
        $element->children = HTML_Parser::parse($content)->children;
      } elseif (is_array($content)) {
        $element->children = $content;
      } else {
        $element->children = [$content];
      }
    }

    return $element;
  }

  public function __debugInfo(): array
  {
    $attrs = [];
    foreach ($this->attributes as $attr) {
      $attrs[] = "{$attr->name}: {$attr->value}";
    }

    return [
        'tag'      => $this->tag,
        'attrs'    => implode(', ', $attrs),
        'children' => count($this->children)
    ];
  }
}

class HTML_Parser_Attribute
{
  public string $name;
  public string|null $value = null;

  public int $offset;
  public string $enclosing;

  public HTML_Parser_Element|null $parent;

  public function remove(): void
  {
    $position = array_search($this, $this->parent->attributes);
    if ($position !== false) {
      unset($this->parent->attributes[$position]);
      $this->parent->attributes = array_values($this->parent->attributes);
      $this->parent = null;
    }
  }
}

class HTML_Parser_Comment extends HTML_Parser_Node
{
  public string $value;
  public int $offset;

  public function render(): string
  {
    return $this->value;
  }

  public function __debugInfo(): array
  {
    return [
        'value' => $this->value
    ];
  }
}

class HTML_Parser_Text extends HTML_Parser_Node
{
  public int $offset;
  public int $length;

  private string|null $_value;

  public function __construct(string $content = null)
  {
    $this->_value = $content;
  }

  public function setContent(string $content): void
  {
    $this->_value = $content;
  }

  public function render(): string
  {
    if (!isset($this->_value)) {
      $this->_value = implode('', array_slice($this->document->chars, $this->offset, $this->length));
    }

    return $this->_value;
  }

  public function __debugInfo(): array
  {
    return [
        'offset' => $this->offset,
        'length' => $this->length,
        'value'  => $this->_value ?? ($this->document ? $this->render() : '#error#')
    ];
  }
}
