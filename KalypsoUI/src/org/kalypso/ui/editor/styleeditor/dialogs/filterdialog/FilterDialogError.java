package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

public class FilterDialogError
{
  private FilterDialogTreeNode node = null;

  private String faultCode = null;

 

  public FilterDialogError( FilterDialogTreeNode m_node, String m_faultCode )
  {
    this.node = m_node;
    this.faultCode = m_faultCode;
  }

  public String getFaultCode()
  {
    return faultCode;
  }

  public void setFaultCode( String m_faultCode )
  {
    this.faultCode = m_faultCode;
  }

  public FilterDialogTreeNode getNode()
  {
    return node;
  }

  public void setNode( FilterDialogTreeNode m_node )
  {
    this.node = m_node;
  }
}