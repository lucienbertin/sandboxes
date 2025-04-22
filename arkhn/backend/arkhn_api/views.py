from django.shortcuts import render
from rest_framework import viewsets, permissions
# from rest_framework.response import Response
from .models import Foo
from .serializers import FooSerializer

# Create your views here.
class FooView(viewsets.ModelViewSet):
    queryset = Foo.objects.all()
    serializer_class = FooSerializer
    permission_classes = [permissions.AllowAny,]