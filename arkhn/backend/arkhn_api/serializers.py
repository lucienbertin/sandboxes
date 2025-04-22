from rest_framework import serializers
from .models import Foo
from django.contrib.auth.hashers import make_password

# class FooObjectSerializer(serializers.ModelSerializer):
#     class Meta:
#         model = Foo
#         fields = ('name')

class FooSerializer(serializers.ModelSerializer):
    class Meta:
        model = Foo
        fields = ('name')
