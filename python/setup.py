#!/usr/bin/env python
# coding: utf-8

from setuptools import setup, find_packages

setup(name='msgpackrpc',
      version='0.0.1dev',
      author='INADA Naoki',
      author_email='songofacandy@gmail.com',
      packages=find_packages(),
      install_requires=['msgpack-python'],
      test_suite='nose.collector',
      tests_require=['nose',],
      )
