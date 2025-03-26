
;;; gptel-tools.el --- Custom tool definitions for gptel -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains custom tool definitions for use with gptel and Anthropic's
;; Claude models. Load this file from your main configuration.

;;; Code:

(gptel-make-tool
 :name "helpful_function_inspect"
 :function (lambda (function-name)
             (message "Looking up function: %s" function-name)
             (condition-case err
                 (save-window-excursion
                   (with-current-buffer (helpful-callable (intern function-name))
                     (message "Found documentation for function: %s" function-name)
                     (gptel-add)
                     (buffer-substring-no-properties (point-min) (point-max))))
               (error 
                (message "Error looking up function %s: %s" function-name (error-message-string err))
                (format "Could not find documentation for function '%s'. Error: %s" 
                        function-name (error-message-string err)))))
 ;; :include t
 :description "Retrieves comprehensive documentation for any Emacs Lisp function.

Purpose:
- Provides in-depth function documentation, source code, and usage examples
- Helps understand how functions work and how to use them correctly
- Shows function arguments, return values, and behavior details

When to use:
- When you need to understand a specific function's behavior
- When troubleshooting code that calls a particular function
- When learning how to implement a function correctly
- When exploring function implementation details

Note: Since this tool adds output directly to the current context, only use it
when necessary and avoid calling it multiple times for the same function."
 :args (list '(:name "function_name"
                     :type string
                     :description "The exact name of the Emacs Lisp function you want to inspect. Enter the function name without quotes or parentheses."))
 :category "introspection")

(gptel-make-tool
 :name "helpful_variable_inspect"
 :function (lambda (variable-name)
             (message "Looking up variable: %s" variable-name)
             (condition-case err
                 (save-window-excursion
                   (with-current-buffer (helpful-variable (intern variable-name))
                     (message "Found documentation for variable: %s" variable-name)
                     (gptel-add)
                     (buffer-substring-no-properties (point-min) (point-max))))
               (error 
                (message "Error looking up variable %s: %s" variable-name (error-message-string err))
                (format "Could not find documentation for variable '%s'. Error: %s" 
                        variable-name (error-message-string err)))))
 ;; :include t 
 :description "Retrieves comprehensive documentation for any Emacs Lisp variable.

Purpose:
- Provides detailed variable information including current value and documentation
- Shows customization options and default values
- Identifies where the variable is defined and referenced
- Displays variable type and constraints

When to use:
- When you need to understand what a variable controls
- When configuring Emacs behavior through variables
- When troubleshooting unexpected behavior related to configuration
- When learning about available customization options

Note: Since this tool adds output directly to the current context, only use it
when necessary and avoid calling it multiple times for the same variable."
 :args (list '(:name "variable_name"
                     :type string
                     :description "The exact name of the Emacs Lisp variable you want to inspect. Enter the variable name without quotes or special syntax."))
 :category "introspection")

(defun gptel-tool-apropos (keyword)
  "Search for Emacs Lisp symbols matching a keyword.

This function searches for functions and variables whose names match the given
keyword. It returns and logs a formatted summary of matching symbols, including
their names and the first line of their documentation.

Arguments:
- KEYWORD: A string to match against symbol names.

Returns:
- A formatted string containing matching functions and variables.
- Logs results to the *gptel-tool-results* buffer.

Side effects:
- Creates or updates the *gptel-tool-results* buffer with search results.
- Displays a message indicating the search results have been logged."
  (message "Searching for symbols matching: %s" keyword)
  (let* ((regexp (regexp-quote keyword))
         (functions (apropos-internal regexp 'functionp))
         (variables (apropos-internal regexp 'boundp))
         (result-text "")
         (buffer-name "*gptel-tool-results*"))
    
    ;; Format function results
    (when functions
      (setq result-text (concat result-text (format "== FUNCTIONS (%d) ==\n" (length functions))))
      (dolist (sym functions)
        (let ((doc (documentation sym t)))
          (setq result-text (concat result-text 
                                    (format "%s: %s\n" 
                                            sym 
                                            (if doc
						(car (split-string doc "\n"))
					      "No documentation")))))))
    
    ;; Format variable results
    (when variables
      (setq result-text (concat result-text 
				(format "\n== VARIABLES (%d) ==\n" (length variables))))
      (dolist (sym variables)
        (let ((doc (documentation-property sym 'variable-documentation)))
          (setq result-text (concat result-text 
                                    (format "%s: %s\n" 
                                            sym 
                                            (if doc
						(car (split-string doc "\n"))
					      "No documentation")))))))
    
    ;; Format the final result string - this will be returned to the LLM
    (setq result-text (if (string= result-text "")
                          (format "No symbols found containing '%s'\n" keyword)
			(concat 
			 (format "Found %d symbols matching '%s':\n\n" 
				 (+ (length functions) (length variables))
				 keyword)
			 result-text)))
    
    ;; Log to debugging buffer without focusing it
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        ;; Set buffer properties if it's new
        (when (= (buffer-size) 0)
          (special-mode)  
          (setq buffer-read-only nil))

	;; Make sure we can write to the buffer
	(setq buffer-read-only nil)
        
        ;; Add content with timestamp
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n" (make-string 70 ?-) "\n\n"))
        (insert (format "Search results for '%s' [%s]\n\n" 
                        keyword 
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert result-text)
        
        ;; Make buffer read-only again but allow quit with 'q'
        (setq buffer-read-only t)
        (local-set-key (kbd "q") 'quit-window))
      
      ;; Just log a message without displaying the buffer
      (message "Results logged in %s buffer" buffer-name))
    
    ;; Always return the complete result text to the LLM
    result-text))

(gptel-make-tool
 :name "find_symbols_by_name"
 :function 'gptel-tool-apropos
 ;; :include t
 :description "Searches for Emacs Lisp symbols (functions and variables) by keyword.

Purpose:
- Discovers relevant functions and variables based on name patterns
- Provides quick overview of available functionality in a specific domain
- Helps identify symbols for further inspection
- Supports exploration of Emacs functionality

When to use:
- When exploring a specific Emacs feature or package
- When you need to find functions related to a concept
- As a first step before using more detailed inspection tools
- When you're unsure which exact function or variable to examine

Output format:
- Returns a formatted text listing of matching symbols
- Lists matching functions with brief descriptions
- Lists matching variables with brief descriptions
- Includes count of matches found
- Results are also logged to *gptel-tool-results* buffer for debugging

Note: When the user explicitly asks to list symbols, you should list them exactly
as provided in the output. For your own discovery purposes without user explicitly
requesting the output, summarize the findings instead."
 :args (list '(:name "keyword"
		     :type string
		     :description "The keyword or pattern to search for in symbol names. This should typically be a feature name (like 'elfeed'), a concept (like 'buffer'), or any text pattern you expect to find in relevant symbol names. The search is case-sensitive and will match partial names."))
 :category "discovery")

(gptel-make-tool
 :name "clean_whole_context"
 :function (lambda ()
             (message "Cleaning all conversation context...")
             (gptel-context-remove-all nil)
             (message "All conversation context has been removed")
             "All conversation context has been cleared. We're starting with a fresh slate.")
 :description "Removes all accumulated context from previous interactions.

Purpose:
- Clears conversation history to start fresh
- Prevents previous conversations from influencing new responses
- Helps when switching between different topics or projects
- Reduces potential confusion from mixing unrelated contexts

When to use:
- After completing a project or code implementation
- When starting a conversation on a new, unrelated topic
- When previous context is causing confusion or incorrect responses
- Before dealing with sensitive or confidential information
- When conversation has grown too long and resource-intensive

Example scenarios:
1. After finishing a large coding project, use this to reset the context
2. When switching between different programming tasks or languages
3. Before beginning a completely new line of inquiry or technical discussion
4. When the AI seems to be mixing up information from previous discussions"
 :category "context-management")

;;; gptel-tools.el ends here
