from datetime import datetime
import re

class ValidationError(Exception):
    def __init__(self, message="Validation failed"):
        super().__init__(message)
        self.message = message

    def __str__(self):
        return self.message

class ValidationField:
    def __init__(self, default=None, blank=False, **kwargs):
        self.default = default
        self.blank = blank
        self.__dict__.update(**kwargs)

    def validate(self, val):
        if val is None and not self.blank:
            raise ValidationError

class CharField(ValidationField):
    def __init__(self, min_length=0, max_length=None, **kwargs):
        super().__init__(**kwargs)
        self.min_length = min_length
        self.max_length = max_length

    def validate(self, val):
        super().validate(val)
        if val is None:
            return
        if not isinstance(val, str):
            raise ValidationError
        if (self.min_length is not None and len(val) < self.min_length) or (self.max_length is not None and len(val) > self.max_length):
            raise ValidationError

class IntegerField(ValidationField):
    def __init__(self, min_value=None, max_value=None, **kwargs):
        super().__init__(**kwargs)
        self.min_value = min_value
        self.max_value = max_value

    def validate(self, val):
        super().validate(val)
        if val is None:
            return
        if not isinstance(val, int):
            raise ValidationError
        if (self.min_value is not None and val < self.min_value) or (self.max_value is not None and val > self.max_value):
            raise ValidationError
            
class BooleanField(ValidationField):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def validate(self, val):
        super().validate(val)
        if  val is None:
            return
        if not isinstance(val, bool):
            raise ValidationError


class DateTimeField(ValidationField):
    def __init__(self, default=None, auto_now=False, **kwargs):
        super().__init__(**kwargs)
        self.auto_now = auto_now
        self._default = default

    def __getattribute__(self, name: str):
        if name == 'default':
            if self._default is None and self.auto_now:
                return datetime.now()
            return self._default
        return super().__getattribute__(name)

    def validate(self, val):
        super().validate(val)
        if val is None:
            return
        if not isinstance(val, datetime):
            raise ValidationError

class EmailField(ValidationField):
    def __init__(self, min_length=0, max_length=None, **kwargs):
        super().__init__(**kwargs)
        self.min_length = min_length
        self.max_length = max_length

    def validate(self, val):
        super().validate(val)
        if val is None:
            return
        if not isinstance(val, str):
            raise ValidationError
        if (self.min_length is not None and len(val) < self.min_length) or (self.max_length is not None and len(val) > self.max_length):
            raise ValidationError
        if not re.match(r"^[^@]+@[^@]+\.[^@]+$", val):
            raise ValidationError

class Model:
    def __init__(self, **kwargs):
        for i in [i for i in dir(self.__class__) if i.startswith('_f_')]:
            attribute = getattr(self.__class__, i) 
            setattr(self, i[3:], attribute.default)
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __init_subclass__(cls) -> None:
        for i in [i for i in dir(cls) if not i.startswith('_')]:
            attribute = getattr(cls, i)
            if isinstance(attribute, (CharField, IntegerField, BooleanField, DateTimeField, EmailField)):
                delattr(cls, i)
                setattr(cls, '_f_' + i, attribute)
        super().__init_subclass__()

    def validate(self):
        for i in [i for i in dir(self.__class__) if i.startswith('_f_') and hasattr(self, i[3:])]:
            class_attribute = getattr(self.__class__, i)
            instance_attribute = getattr(self, i[3:])
            class_attribute.validate(instance_attribute)