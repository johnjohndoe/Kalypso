/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * @author F.Lindemann
 *  
 */
public class FormatDisplayPanel
{

  private Composite composite = null;

  private String format = null;

  private String label = null;

  public FormatDisplayPanel( Composite parent, String m_label, String m_format )
  {
    setLabel( m_label );
    setFormat( m_format );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 190;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  private void init()
  {
    final Text text = new Text( composite, SWT.READ_ONLY );
    FormData textData = new FormData();
    textData.height = 17;
    textData.width = 100;
    textData.left = new FormAttachment( 340, 1000, 0 );
    textData.top = new FormAttachment( 10, 1000, 0 );
    text.setLayoutData( textData );
    text.setText( format );

    Label urlLabel = new Label( composite, SWT.NULL );
    FormData urlLabelData = new FormData();
    urlLabelData.height = 15;
    urlLabelData.width = 242;
    urlLabelData.left = new FormAttachment( 0, 1000, 0 );
    urlLabelData.top = new FormAttachment( 100, 1000, 0 );
    urlLabel.setLayoutData( urlLabelData );
    urlLabel.setText( label );
  }

  public String getFormat()
  {
    return format;
  }

  public void setFormat( String m_format )
  {
    this.format = m_format;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }
}