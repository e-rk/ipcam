import Distribution.Simple
import Distribution.Simple.Gresource
import Distribution.Simple.I18N.GetText

main = defaultMainWithHooks $ cabalGresourceHooks $ installGetTextHooks simpleUserHooks
