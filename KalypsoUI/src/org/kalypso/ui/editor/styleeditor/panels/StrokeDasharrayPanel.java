package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;

/**
 * @author F.Lindemann
 *  
 */
public class StrokeDasharrayPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  private Text lineInput = null;

  private Text spaceInput = null;

  private float lineValue = 0;

  private float spaceValue = 0;

  public StrokeDasharrayPanel( Composite parent, String m_label, float[] values )
  {
    setLabel( m_label );
    if( values != null && values.length >= 2 )
    {
      lineValue = values[0];
      spaceValue = values[1];
    }
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 203;
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

    Label okButton = new Label( composite, SWT.PUSH );
    okButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_OK.createImage() );
    FormData okButtonData = new FormData();
    okButtonData.height = 18;
    okButtonData.width = 20;
    okButtonData.left = new FormAttachment( 910, 1000, 0 );
    okButtonData.top = new FormAttachment( 100, 1000, 0 );
    okButton.setLayoutData( okButtonData );
    okButton.setToolTipText( MessageBundle.STYLE_EDITOR_OK );

    spaceInput = new Text( composite, SWT.BORDER );
    FormData offsetInputData = new FormData();
    offsetInputData.height = 10;
    offsetInputData.width = 16;
    offsetInputData.left = new FormAttachment( 760, 1000, 0 );
    offsetInputData.top = new FormAttachment( 100, 1000, 0 );
    spaceInput.setLayoutData( offsetInputData );
    spaceInput.setText( "" + spaceValue );

    Label spaceLabel = new Label( composite, SWT.NULL );
    FormData spaceLabelData = new FormData();
    spaceLabelData.height = 16;
    spaceLabelData.width = 5;
    spaceLabelData.left = new FormAttachment( 740, 1000, 0 );
    spaceLabelData.top = new FormAttachment( 100, 1000, 0 );
    spaceLabel.setLayoutData( spaceLabelData );
    spaceLabel.setText( "/" );

    lineInput = new Text( composite, SWT.BORDER );
    FormData lineInputData = new FormData();
    lineInputData.height = 10;
    lineInputData.width = 16;
    lineInputData.left = new FormAttachment( 590, 1000, 0 );
    lineInputData.top = new FormAttachment( 100, 1000, 0 );
    lineInput.setLayoutData( lineInputData );
    lineInput.setText( "" + lineValue );

    final Label lineLabel = new Label( composite, SWT.NULL );
    FormData lineLabelData = new FormData();
    lineLabelData.height = 15;
    lineLabelData.width = 50;
    lineLabelData.left = new FormAttachment( 340, 1000, 0 );
    lineLabelData.top = new FormAttachment( 100, 1000, 0 );
    lineLabel.setLayoutData( lineLabelData );
    lineLabel.setText( MessageBundle.STYLE_EDITOR_LINE_SPACE );

    Label offsetLabel = new Label( composite, SWT.NULL );
    FormData offsetLabelData = new FormData();
    offsetLabelData.height = 15;
    offsetLabelData.width = 242;
    offsetLabelData.left = new FormAttachment( 0, 1000, 0 );
    offsetLabelData.top = new FormAttachment( 100, 1000, 0 );
    offsetLabel.setLayoutData( offsetLabelData );
    offsetLabel.setText( label );

    okButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        Float lineFloat = null;
        Float spaceFloat = null;
        try
        {
          lineFloat = new Float( getLineInput().getText() );
          spaceFloat = new Float( getSpaceInput().getText() );
          setLineValue( lineFloat.floatValue() );
          if( getLineValue() <= 0.0 )
          {
            setLineValue( 1f );
            lineLabel.setText( "" + getLineValue() );
          }
          setSpaceValue( spaceFloat.floatValue() );
          fire();
        }
        catch( NumberFormatException nfe )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
              .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
              MessageBundle.STYLE_EDITOR_ERROR_NUMBER );
          errorDialog.showError();
          getLineInput().setText( "" + getLineValue() );
          getSpaceInput().setText( "" + getSpaceValue() );
        }
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );
  }

  public float[] getValue()
  {
    float returnArray[] =
    { lineValue, spaceValue };
    return returnArray;
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

  public Text getLineInput()
  {
    return lineInput;
  }

  public void setLineInput( Text m_lineInput )
  {
    this.lineInput = m_lineInput;
  }

  public float getLineValue()
  {
    return lineValue;
  }

  public void setLineValue( float m_lineValue )
  {
    this.lineValue = m_lineValue;
  }

  public Text getSpaceInput()
  {
    return spaceInput;
  }

  public void setSpaceInput( Text m_spaceInput )
  {
    this.spaceInput = m_spaceInput;
  }

  public float getSpaceValue()
  {
    return spaceValue;
  }

  public void setSpaceValue( float m_spaceValue )
  {
    this.spaceValue = m_spaceValue;
  }
}