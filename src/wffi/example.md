# Name of API service

The first h1 section is an introduction to the overall web service.

Also the following line is special; the endpoint for the service:

Endpoint: http://foo/bar

The remaining h1 sections can be used to document and specify each API
function of the web service.

# Name of API function

The section level 1 heading above is used as the name of the API
function.

Documentation text here. Use full markdown formatting.

## Optional subsection(s) with more documentation

If you need more subsections for documentation, you may use them.

## Request

A section level 2 named "Request" is special. It may contain an
HTTP request template, inside of a code block defined with ````
markers. Notice that's **4** ticks not 3. For instance:

````
GET /user/{user}/items/{item}
    ?qp1={}
    &[qp2={}]
Header1: {}
Header2: {alias}
````

Other than the code block, the rest of the section is purely for
documentation.

If you need another code block in this section that is _not_ the
template, then mark it using an indent of four spaces:

    Here's a code block using
    the indented spaces style.
    It won't be confused with
    the template.

Or, mark it using just the usual 3 ticks (instead of 4):

```
Here's a code block using
the indented spaces style.
It won't be confused with
the template.
```

## Response:

Likewise, a section level 2 named "Response" is special. It may
contain an HTTP response template.  For instance:

````
HTTP/1.1 200 OK
Date: {}
Content-Type: {}
Content-Length: {}
````

## Other sections

There may be other sections for documentation purposes.
