package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 *  
 */
public class TextInputPanel
{

  private Composite composite = null;

  private String labelText = null;

  private Text text = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  public TextInputPanel( Composite parent, String m_label, String m_text )
  {
    setLabel( m_label );
    setLabelText( m_text );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 195;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    text = new Text( composite, SWT.BORDER );
    text.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );

    FormData textData = new FormData();
    textData.height = 10;
    textData.width = 90;
    textData.left = new FormAttachment( 340, 1000, 0 );
    textData.top = new FormAttachment( 10, 1000, 0 );
    text.setLayoutData( textData );
    if( labelText != null )
      text.setText( labelText );

    Label okButton = new Label( composite, SWT.PUSH );
    okButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_OK.createImage() );
    FormData okButtonData = new FormData();
    okButtonData.height = 15;
    okButtonData.width = 22;
    okButtonData.left = new FormAttachment( 890, 1000, 0 );
    okButtonData.top = new FormAttachment( 100, 1000, 0 );
    okButton.setLayoutData( okButtonData );
    okButton.setToolTipText( MessageBundle.STYLE_EDITOR_OK );

    okButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        setLabelText( getText().getText() );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );

    Label urlLabel = new Label( composite, SWT.NULL );
    FormData urlLabelData = new FormData();
    urlLabelData.height = 15;
    urlLabelData.width = 242;
    urlLabelData.left = new FormAttachment( 0, 1000, 0 );
    urlLabelData.top = new FormAttachment( 100, 1000, 0 );
    urlLabel.setLayoutData( urlLabelData );
    urlLabel.setText( label );
  }

  public String getLabelText()
  {
    return labelText;
  }

  public void setInputText( String textInput )
  {
    labelText = textInput;
    text.setText( textInput );
  }

  // sets the inputField to a default state
  public void reset()
  {
    text.setText( "" );
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[i + 1] ).valueChanged( event );
      }
    }
  }

  public Composite getComposite()
  {
    return composite;
  }

  public void setComposite( Composite m_composite )
  {
    this.composite = m_composite;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public Text getText()
  {
    return text;
  }

  public void setText( Text m_text )
  {
    this.text = m_text;
  }

  public void setLabelText( String m_labelText )
  {
    this.labelText = m_labelText;
  }
}