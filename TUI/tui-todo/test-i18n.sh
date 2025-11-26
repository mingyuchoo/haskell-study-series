#!/bin/bash

echo "Testing Korean (default)..."
rm -f todos.db
timeout 1 stack exec tui-todo-exe 2>&1 | head -20 || true

echo ""
echo "================================"
echo "To test English, modify src/Lib.hs:"
echo "Change: app = appWithLanguage I18n.Korean"
echo "To:     app = appWithLanguage I18n.English"
echo "Then rebuild with: stack build"
echo "================================"
