# cl-llm-agent: an Example program in the book "Loving Common Lisp, or the Savvy"

Chapter: "Agents Orchestrating LLM Tool Use"

Read the book free online, or by it:

    https://leanpub.com/lovinglisp/read

This example relies on two more of my projects; make sure to clone these into~/quicklisp/local-projects/

    https://github.com/mark-watson/tavily
    https://github.com/mark-watson/gemini
	
## Sample output

```text
* (ql:quickload :cl-llm-agent)
* (load "test.lisp")
Context fetched from agent
  current-task: researching restaurants
  user-location: Paris
Original context
  current-task: researching restaurants
  user-location: Paris
&* * agent-converse: Search the web to find information on AI advancements.
Context at start of agent-converse call
  current-task: researching restaurants
  user-location: Paris

LLM Response: {"action": "tool-search-web", "parameters": {"query": "recent advancements in Artificial Intelligence"}}


Cleaned LLM Response: {"action": "tool-search-web", "parameters": {"query": "recent advancements in Artificial Intelligence"}}
* agent-converse: action-request = ((ACTION . tool-search-web) (PARAMETERS (QUERY . recent advancements in Artificial Intelligence)))
*   (assoc :ACTIONS action-request :test #'equal): NIL
*   (assoc :ACTION action-request :test #'equal): (ACTION . tool-search-web)

debug: actions: (((ACTION . tool-search-web) (PARAMETERS (QUERY . recent advancements in Artificial Intelligence))))
* tools.lisp: execute-tool tool-search-web
* tool-search-web query: recent advancements in Artificial Intelligence

* Calling tavily-search with query: recent advancements in Artificial Intelligence

"Over the last few years, artificial intelligence (AI) has worked its way into every area of our lives. As the founder of a technology investment firm, I’ve seen firsthand just how much AI has advanced in such a short period of time. The development of AlphaFold, an AI system that descended from AlphaGo, was originally a deep learning model trained to beat human beings at the board game Go. When AlphaGo was released five years ago, few predicted that this deep learning model would one day lead to a revolutionary discovery in molecular biology. As AI continues to develop at an exponential rate, it is transforming the business world. Leveraging AI technologies can be a source of durable competitive advantage for businesses.
This report from the Congressional Research Service provides a brief background on AI technologies and recent advances, such as generative AI, and their benefits and risks. It also discusses current federal laws, perspectives on regulating AI, and other considerations for the 118th Congress.
Jan. 15, 2025 — A new initiative is challenging the conversation around the direction of artificial intelligence (AI). Jan. 16, 2025 — Researchers have developed a novel 6D pose dataset designed to improve robotic grasping accuracy and adaptability in industrial settings. Dec. 19, 2024 — Researchers developed a laser-based artificial neuron that fully emulates the functions, dynamics and information processing of a biological graded neuron, which could lead to new breakthroughs in ... Dec. 9, 2024 — Imagine an artificial intelligence (AI) model that can watch and understand moving images with the subtlety of a human brain. Dec. 2, 2024 — A research team has taken inspiration from the brains of insects and animals for more energy-efficient robotic ...
We furthered our industry-leading research in AI safety, developing new tools and techniques and integrating these advances into our latest models. We expanded SynthID’s capabilities to watermarking AI-generated text in the Gemini app and web experience, and video in Veo. To help increase overall transparency online, not just with content created by Google gen AI tools, we also joined the Coalition for Content Provenance and Authenticity (C2PA) as a steering committee member and collaborated on a new, more secure version of the technical standard, Content Credentials. Google Cloud #### New tools to help retailers build gen AI search and agents By Carrie Tharp Jan 12, 2025
The 3 Most Important AI Innovations of 2023 | TIME TIME 2030 This year was the first time that the public gained access to powerful multimodal AI models. “It’s early days in this transition, and when you start really digesting a lot of video and other things like that, these systems will start having a much more grounded understanding of the world.” In an interview with TIME in November, OpenAI CEO Sam Altman said multimodality in the company’s new models would be one of the key things to watch out for next year. “With constitutional AI, you’re explicitly writing down the normative premises with which your model should approach the world,” Jack Clark, Anthropic’s head of policy, told TIME in August.
" 
Current Task from Context: researching restaurants
&* * agent-converse: Read the file 'test.txt' in the current directory and summarize the text in the file.
Context at start of agent-converse call
  current-task: researching restaurants
  user-cuisine-preference: Italian
  user-location: Paris

LLM Response: {"action": "tool_read_file", "parameters": {"filename": "test.txt"}}


Cleaned LLM Response: {"action": "tool-read-file", "parameters": {"filename": "test.txt"}}
* agent-converse: action-request = ((ACTION . tool-read-file) (PARAMETERS (FILENAME . test.txt)))
*   (assoc :ACTIONS action-request :test #'equal): NIL
*   (assoc :ACTION action-request :test #'equal): (ACTION . tool-read-file)

debug: actions: (((ACTION . tool-read-file) (PARAMETERS (FILENAME . test.txt))))
* tools.lisp: execute-tool tool-read-file
* tool-read-file in path test.txt

"Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows, and is on average the third-brightest natural object in the night sky after the Moon and Venus.
" 
```

