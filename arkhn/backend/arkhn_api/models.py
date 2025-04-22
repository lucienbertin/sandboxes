from django.db import models

# Create your models here.
class Foo(models.Model):
    name = models.CharField(null=True)
    def __str__(self):
        return f'Foo {self.name}'