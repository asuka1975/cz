/// ソースファイル内の位置を表すバイトオフセット範囲。
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
}

/// 全コンパイルフェーズ共通の診断情報コレクタ。
#[allow(dead_code)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

#[allow(dead_code)]
impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn error(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            span,
            message: message.into(),
        });
    }

    pub fn warning(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Warning,
            span,
            message: message.into(),
        });
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    /// バイトオフセットから行番号 (1-based) を計算する。
    fn offset_to_line(source: &str, offset: u32) -> usize {
        let offset = offset as usize;
        source[..offset.min(source.len())]
            .bytes()
            .filter(|&b| b == b'\n')
            .count()
            + 1
    }

    /// 診断メッセージをフォーマットして stderr 向け文字列にする。
    /// 現行テストとの互換性のため、行番号付きの単純なフォーマットを使用する。
    pub fn render(&self, source: &str) -> Vec<String> {
        self.diagnostics
            .iter()
            .map(|d| {
                let line = Self::offset_to_line(source, d.span.start);
                let prefix = match d.severity {
                    Severity::Error => "意味解析エラー",
                    Severity::Warning => "警告",
                };
                format!("{}: {}行目: {}", prefix, line, d.message)
            })
            .collect()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    /// ソースとスパンから行番号を計算する公開ユーティリティ。
    pub fn span_to_line(source: &str, span: Span) -> usize {
        Self::offset_to_line(source, span.start)
    }

    /// ソースが利用できない場合のフォールバック (オフセットをそのまま使用)。
    pub fn span_to_line_static(_source: &str, span: Span) -> u32 {
        span.start
    }
}
