# 개요
* 다변량분석 내용 정리
* R과 Python 코드를 동시에 사용
* R의 경우 코드만 등재. Markdown으로 추가 설명
* Python의 경우, ipynb를 이용해 설치

# 내용
1. 다변량 정규분포
2. 주성분분석
3. 요인분석 (인자분석)
4. 정준상관분석
5. 다항분포
6. 대응분석
7. 군집분석
8. 판별분석

# 실행방법
* 패키지가 없는 경우, 또는 패키지를 설치하기 힘든 경우도 존재한다.
* 이럴 때는 파이썬에서 제공하는 가상환경 라이브러리(venv)를 쓴다.

1. venv 설치
터미널을 켜고, 다음 코드를 입력.
```bash
python3 -m venv myenv
source myenv/bin/activate
```

* venv 해제
```bash
deactivate
```

* venv 재접속
```bash
source myenv/bin/activate
```

2. 패키지 설치
```bash
pip install --upgrade pip
pip install -r requirements.txt
```
`requirements.txt` 안에 모든 패키지가 다 들어있다.
`requirements.txt`는 다음과 같이 업데이트 한다.
```bash
pip freeze > requirements.txt
```

# gitignore
* 최상단 디렉토리에 .gitignore 파일을 만들자.
* 최소한 이 정도는 추가하는 것이 좋다.
```
/myenv
```