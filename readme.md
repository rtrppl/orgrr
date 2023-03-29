# orgrr 

A number of hacks that provide a replacement for the core functions of org-roam v1. Orgrr does not use any database but instead is built around rg (ripgrep), hashtables and regex.


- use ChatGPT to generate text, having full control over system and user prompts ([demo](#chatgpt-in-org-mode))
- generate images with a text prompt using DALL-E  ([demo](#dall-e-in-org-mode))
- generate image variations of an input image ([demo](#image-variations))

Implemented in pure Emacs Lisp, no external dependencies required (except currently for image variations[^1]).

_Note: In order to use this you'll need [rg](https://platform.openai.com/) installed on your machine. The easiest way to do so might be homebrew, i.e. "brew install rg"._

------------------------------

## Table of Contents

- [Features](#features)
- [Demos](#demos)
    - [ChatGPT in org-mode](#chatgpt-in-org-mode)
    - [DALL-E in org-mode](#dall-e-in-org-mode)
    - [Image variations](#image-variations)
- [Options](#options)
- [Setup](#setup)
    - [Melpa](#melpa)
    - [Straight.el](#straightel)
    - [Manual](#manual)
- [FAQ](#faq)


## Features

### `#+begin_ai...#+end_ai` special blocks

Similar to org-babel, these blocks demarcate input (and for ChatGPT also output) for the AI model. You can use it for AI chat, text completion and text -> image generation. See [options](#options) below for more information.

Create a block like

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

and press `C-c C-c`. The Chat input will appear inline and once the response is complete, you can enter your reply and so on. See [the demo](#chatgpt-in-org-mode) below. You can press `C-g` while the ai request is running to cancel it.

You can also modify the _system_ prompt and other parameters used. The system prompt is injected before the user's input and "primes" the model to answer in a certain style. For example you can do:

```org
#+begin_ai :max-tokens 250
[SYS]: Act as if you are a powerful medival king.
[ME]: What will you eat today?
#+end_ai
```

This will result in an API payload like

```json
{
  "messages": [
    {
      "role": "system",
      "content": "Act as if you are a powerful medival king."
    },
    {
      "role": "user",
      "content": "What will you eat today?"
    }
  ],
  "model": "gpt-3.5-turbo",
  "stream": true,
  "max_tokens": 250,
  "temperature": 1.2
}
```

For some prompt ideas see for example [Awesome ChatGPT Prompts](https://github.com/f/awesome-chatgpt-prompts).

When generating images using the `:image` flag, images will appear underneath the ai block inline. Images will be stored (together with their prompt) inside `org-ai-image-directory` which defaults to `~/org/org-ai-images/`.

### Image variation

You can also use an existing image as input to generate more similar looking images. The `org-ai-image-variation` command will prompt for a file path to an image, a size and a count and will then generate as many images and insert links to them inside the current `org-mode` buffer. Images will be stored inside `org-ai-image-directory`. See the [demo](#image-variations) below.
