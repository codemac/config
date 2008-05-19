@echo off
rem ---
rem --- common install batch file for Meadow & NTEmacs
rem ---  1999/07/07, Masaki YATSU mailto:yatsu@aurora.dti.ne.jp
rem ---              cmail ML member
rem ---  modified 1999/12/01, Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem ---  modified 2000/12/26, Takeshi Morishima mailto:tm@interaccess.com
rem ---  date $Date: 2001/02/01 03:19:36 $
rem ---  version $Id: makeit.bat,v 1.1 2001/02/01 03:19:36 minakaji Exp $

set ELISPMK_APP=apel

rem --- Japanese Comments:
rem ---
rem --- ����
rem ---   �����ɂ��Ă� make1.bat �̃R�����g���Q�Ƃ��Ă��������D
rem ---   makeit.bat �́A�C���X�g�[���̊��ϐ���ݒ肵�����
rem ---   make1.bat ���ďo���ăC���X�g�[�����s���܂��B
rem ---
rem --- �ϐ��ݒ�
rem ---   ���̃R�����g�̂��Ƃɂ��� PREFIX, EMACS, EXEC_PREFIX, LISPDIR,
rem ---   INFODIR, VERSION_SPECIFIC_LISPDIR �̊e�ϐ����C���g���̊���
rem ---   �K���ɍ����Đݒ肵�Ă��������D
rem ---   ���ɁCEMACS �̒l���C
rem ---     Windows95/98 �𗘗p����Ă������ meadow95.exe
rem ---     WindowsNT4.0 �𗘗p����Ă������ meadownt.exe
rem ---     NTEmacs �𗘗p����Ă������ emacs.exe
rem ---   ���w�肷��̂�Y��Ȃ��悤�ɁD
rem ---
rem ---   �K�X�w�肪�I���� makeit.bat �͉��̂����ꂩ�̃t�@�C���Ƃ���
rem ---   �R�s�[���Ă����Ƃ������D�悵�Ď��s���܂��B(�A�b�v�O���[�h
rem ---   �̍ۂ� makeit.bat ���ĕҏW����K�v������܂���.) �D�揇��:
rem ---
rem ---     1-1. %HOME%\.elispmk.%ELISPMK_APP%.bat
rem ---     1-2. %HOME%\elisp\elispmk.%ELISPMK_APP%.bat
rem ---     1-3. %HOME%\config\elispmk.%ELISPMK_APP%.bat
rem ---     1-4. c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-5. c:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-6. d:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---
rem ---     2-1. %HOME%\.elispmk.bat
rem ---     2-2. %HOME%\elisp\elispmk.bat
rem ---     2-3. %HOME%\config\elispmk.bat
rem ---     2-4. c:\Program Files\Meadow\elispmk.bat
rem ---     2-5. c:\Meadow\elispmk.bat
rem ---     2-6. d:\Meadow\elispmk.bat
rem ---
rem ---   �ƂȂ�܂��B
rem ---
rem --- English Comments:
rem ---
rem --- Arguments
rem ---   Please refer to comment section of make1.bat. Makeit.bat
rem ---   will perform installation procedure by executing make1.bat.
rem ---
rem --- Specifying variables
rem ---   After this comment section, PREFIX, EMACS, EXEC_PREFIX,
rem ---   LISPDIR, INFODIR, VERSION_SPECIFIC_LISPDIR is defined using
rem ---   'set' batch command. Please specify them appropriately
rem ---   according to your Emacs environment. Especially remember to set
rem ---   the EMACS variable to meadow95.exe if you use Meadow on
rem ---   Windows95/98, or to meadownt.exe if you use Meadow on
rem ---   WindowsNT4.0, or to emacs.exe if you use NTEmacs.
rem ---
rem ---   After modification, you may make a copy of makeit.bat as a pre-
rem ---   configured file as one of the following name. Any future
rem ---   execution of makeit.bat will automatically use this pre-
rem ---   configured batch file instead of makeit.bat itself. (When
rem ---   upgrading new distribution file for example, you do not have to
rem ---   make modification to makeit.bat again.) A pre-configured batch
rem ---   file is searched in order listed below:
rem ---
rem ---     1-1. %HOME%\.elispmk.%ELISPMK_APP%.bat
rem ---     1-2. %HOME%\elisp\elispmk.%ELISPMK_APP%.bat
rem ---     1-3. %HOME%\config\elispmk.%ELISPMK_APP%.bat
rem ---     1-4. c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-5. c:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---     1-6. d:\Meadow\elispmk.%ELISPMK_APP%.bat
rem ---
rem ---     2-1. %HOME%\.elispmk.bat
rem ---     2-2. %HOME%\elisp\elispmk.bat
rem ---     2-3. %HOME%\config\elispmk.bat
rem ---     2-4. c:\Program Files\Meadow\elispmk.bat
rem ---     2-5. c:\Meadow\elispmk.bat
rem ---     2-6. d:\Meadow\elispmk.bat

rem --- �ϐ��ݒ�̗� (Example of variable definition)
rem --- c:\usr\Meadow �ɃC���X�g�[������Ă��� 1.10 �� Meadow ���g�p
rem --- ���Ă���ꍇ�̐ݒ��. (An example of variable definition. In
rem --- this example, Meadow 1.10 installed in c:\usr\Meadow directory
rem --- is used.)
rem ---   set PREFIX=c:\usr\Meadow
rem ---   set EMACS=%PREFIX%\1.10\bin\meadow95.exe
rem ---   set EXEC_PREFIX=
rem ---   set LISPDIR=%PREFIX%\site-lisp
rem ---   set VERSION_SPECIFIC_LISPDIR=%PREFIX%\1.10\site-lisp
rem ---   set DEFAULT_MAKE_ARG=elc
rem --- ���S�̂��߃f�t�H���g�̒l�͂��ׂċ󕶎���ɂȂ��Ă��܂��B���g��
rem --- �̃V�X�e���ɂ��킹�Ă����̕ϐ����w�肵�Ă��������B(To take a
rem --- safe side, default values are all set to null strings. Please
rem --- specify these variables accordingly for your system.)
rem --- �Ȃ��ADEFAULT_MAKE_ARG �ɉ\�Ȓl�� make1.bat ���䗗���������B
rem --- (Please see make1.bat for possible values of DEFAULT_MAKE_ARG.)

set PREFIX=
set EMACS=
set LISPDIR=
set DEFAULT_MAKE_ARG=


rem --- makeit.bat ������Ă΂�Ă���ꍇ�͍ċA�Ăяo�������� make1 �����s
if not "%ELISPMK%"=="" goto execsubmk

rem ---
set ELISPMK=%HOME%\.elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\elisp\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\config\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK="c:\Program Files\Meadow\elispmk.%ELISPMK_APP%.bat"
if exist %ELISPMK% goto execelmkb
set ELISPMK=c:\Meadow\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=d:\Meadow\elispmk.%ELISPMK_APP%.bat
if exist %ELISPMK% goto execelmkb
rem ---
set ELISPMK=%HOME%\.elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\elisp\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=%HOME%\config\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK="c:\Program Files\Meadow\elispmk.bat"
if exist %ELISPMK% goto execelmkb
set ELISPMK=c:\Meadow\elispmk.bat
if exist %ELISPMK% goto execelmkb
set ELISPMK=d:\Meadow\elispmk.bat
if exist %ELISPMK% goto execelmkb

echo ----
echo INFORMATIVE: No pre-configured batch (e.g. ~/.elispmk.bat
echo INVORMATIVE: or ~/.elispmk.%ELISPMK_APP%.bat) found.
echo INFORMATIVE: You may create one for your convenience.
echo INFORMATIVE: See comments in makeit.bat.
echo ----

:execsubmk
set ELISPMK=
rem --- %EMACS% ���ꍇ�̓G���[�I������
if "%EMACS%"=="" goto errnotspecified
if not exist "%EMACS%" goto errnonexistent

rem --- MAKE1.BAT Control
set SUBMAKEOK=OK

echo ----
echo Executing make1.bat in the current directory using the folloiwing env.
echo HOME=%HOME%
echo PREFIX=%PREFIX%
echo EMACS=%EMACS%
echo EXEC_PREFIX=%EXEC_PREFIX%
echo LISPDIR=%LISPDIR%
echo INFODIR=%INFODIR%
echo VERSION_SPECIFIC_LISPDIR=%VERSION_SPECIFIC_LISPDIR%
echo ----

set ARG=%1
if "%ARG%"=="" set ARG=%DEFAULT_MAKE_ARG%

echo Executing .\make1.bat with argument=%ARG%
.\make1.bat %ARG%

echo Error: for some reason .\make1.bat could not be executed.
echo Please check if .\make1.bat exists and correct.
goto pauseend

:execelmkb
echo ----
echo Found %ELISPMK%. Executing it...
echo ----
%ELISPMK% %1
echo Error: for some reason %ELISPMK% could not be executed.
echo Please check if ELISPMK=%ELISPMK% exists and correct.
goto printenv

rem --- %EMACS% ���ݒ肳��Ă��Ȃ�
:errnotspecified
echo Error: Environment variable EMACS is not specified.
goto printenv

rem --- %EMACS% �ɐݒ肳��Ă���t�@�C�������݂��Ȃ�
:errnonexistent
echo Error: EMACS=%EMACS% does not exist.

:printenv
echo ----
echo Check correctness of the following environment variables.
echo HOME=%HOME%
echo PREFIX=%PREFIX%
echo EMACS=%EMACS%
echo EXEC_PREFIX=%EXEC_PREFIX%
echo LISPDIR=%LISPDIR%
echo INFODIR=%INFODIR%
echo VERSION_SPECIFIC_LISPDIR=%VERSION_SPECIFIC_LISPDIR%
echo DEFAULT_MAKE_ARG=%DEFAULT_MAKE_ARG%
echo See comments in makeit.bat and make1.bat for setup instruction.
echo ----

:pauseend
echo Type any key when you're done reading the error message.
pause

rem --- end of makeit.bat
:end
