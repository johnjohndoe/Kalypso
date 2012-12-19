package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

public enum ImportMode
{
  overwrite( Messages.getString( "ImportAttachmentsDocumentsData.0" ) ), //$NON-NLS-1$
  skip( Messages.getString( "ImportAttachmentsDocumentsData.1" ) ); //$NON-NLS-1$

  private final String m_label;

  private ImportMode( final String label )
  {
    m_label = label;
  }

  @Override
  public String toString( )
  {
    return m_label;
  }
}