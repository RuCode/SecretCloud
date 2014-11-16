unit AppStrings;

{$mode objfpc}{$H+}

interface

const
  VIEW_INIT           = 'Инициализация';
  VIEW_NO_SELECTED    = 'Нет выделенных элементов';
  VIEW_SEVERAL_SELECT = 'Выделено несколько файлов';
  VIEW_CREATION_DATE  = 'Дата создания: ';

  DIALOG_WARNING      = 'Внимание';
  DIALOG_ERROR        = 'Ошибка';
  DIALOG_ENTER        = 'Ввод';

  TEXT_PASSWORD_WRONG = 'Вы ввели не верный пароль, пожалуйста повторите ввод, осталось попыток: ';
  TEXT_PREVIEW        = 'Просмотр файлов требует их расшифровки, продолжить?';
  TEXT_DELETE         = 'Вы действительно хотите удалить файл?';
  TEXT_ENTER_PASSWORD = 'Введите пароль для расшифровки БД:';
  TEXT_DISABLED_PASS  = 'Выбранное Вами действие не возможно выполнить из-за ранее не верно введеного пароля...';

function UTF(Text: String = ''): String; inline;

implementation

function UTF(Text: String = ''): String; inline;
begin
  {$IFDEF UNIX}
    Result:= Text;
  {$ENDIF}
  {$IFDEF WINDOWS}
    Result:= Utf8ToAnsi(Text);
   {$ENDIF}
end;

end.

