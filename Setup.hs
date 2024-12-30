import Distribution.Simple
import Distribution.Simple.I18N.GetText

main = defaultMainWithHooks $ installGetTextHooks simpleUserHooks
