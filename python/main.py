from models import Posts
from tortoise.contrib.sanic import register_tortoise
from sanic import Sanic, response

app = Sanic(__name__)

register_tortoise(
    app, db_url="postgres://postgres:postgres@localhost:5432/postgres", modules={"models": ["models"]}, generate_schemas=True
)

@app.get("/posts")
async def get_posts(_request):
    posts = await Posts.all()
    return response.json(list(map(lambda p:p.toJSON(), posts)))

@app.get("/post/<pk:int>")
async def get_post(_request, pk):
    post = await Posts.get(pk=pk)
    return response.json(post.toJSON())

@app.post("/posts")
async def post_post(request):
    data = request.json
    new_post = Posts(
        title=data.get("title"),
        body=data.get("body"),
        published=False,
        author="tba",
    )
    await new_post.save()
    return response.empty(status=201, headers={"Location": f'/post/{new_post.id}'})

@app.delete("/post/<pk:int>")
async def delete_post(_request, pk):
    post = await Posts.get(pk=pk)
    if post.published:
        return response.empty(status=409)
    await post.delete()
    return response.empty()

@app.patch("/post/<pk:int>")
async def patch_post(request, pk):
    post = await Posts.get(pk=pk)
    data = request.json

    if post.published:
        return response.empty(status=409)
    
    if "title" in data:
        post.title = data.get("title")
    if "body" in data:
        post.body = data.get("body")
    await post.save()
    return response.empty()

@app.post("/post/<pk:int>/publish")
async def publish_post(_request, pk):
    post = await Posts.get(pk=pk)
    post.published = True
    await post.save()
    return response.empty()

if __name__ == "__main__":
    app.run(port=5000)