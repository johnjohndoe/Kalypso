package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

public class FilterDialogException extends Exception
{
  private FilterDialogError error = null;

  public FilterDialogException()
  {/**/}

  public FilterDialogException( FilterDialogError m_error )
  {
    super( m_error.getFaultCode() );
    this.error = m_error;
  }

  public FilterDialogError getError()
  {
    return error;
  }
}