```php
  $dom = HTML_Parser::parse($html);

        foreach ($dom->children as $node) {
            if ($node instanceof HTML_Parser_Element) {
                $this->_processNode($node, $html, $path, $getTranslation);
            }
        }

        // Remove i18n attributes
        if ($getTranslation) {
            $dom->walk(
                function ($node) {
                    if ($node instanceof HTML_Parser_Element && $node->hasAttribute('i18n')) {
                        $node->hasAttribute('i18n')->remove();
                    }
                }
            );
        }

  return $dom->render();
````