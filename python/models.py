# ./models.py
from tortoise import Model, fields


class Posts(Model):
    id = fields.IntField(pk=True)
    title = fields.TextField()
    body = fields.TextField()
    published = fields.BooleanField()
    author = fields.TextField()


    def __str__(self):
        return self.title
    def toJSON(self):
        return {
            "id": self.id,
            "title": self.title,
            "body":self.body,
            "published": self.published,
            "author": self.author,
        }