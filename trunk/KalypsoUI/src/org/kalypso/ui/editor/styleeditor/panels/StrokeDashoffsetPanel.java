package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;

/**
 * @author F.Lindemann
 *  
 */
public class StrokeDashoffsetPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  private Text offsetInput = null;

  private float value = 0;

  public StrokeDashoffsetPanel( Composite parent, String m_label, float m_value )
  {
    setLabel( m_label );
    setValue( m_value );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 200;
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

    offsetInput = new Text( composite, SWT.BORDER );
    FormData offsetInputData = new FormData();
    offsetInputData.height = 10;
    offsetInputData.width = 20;
    offsetInputData.left = new FormAttachment( 340, 1000, 0 );
    offsetInputData.top = new FormAttachment( 100, 1000, 0 );
    offsetInput.setLayoutData( offsetInputData );
    offsetInput.setText( "0.0" );

    Button okButton = new Button( composite, SWT.PUSH );
    FormData okButtonData = new FormData();
    okButtonData.height = 18;
    okButtonData.width = 20;
    okButtonData.left = new FormAttachment( 540, 1000, 0 );
    okButtonData.top = new FormAttachment( 100, 1000, 0 );
    okButton.setLayoutData( okButtonData );
    okButton.setText( "Ok" );
    okButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        Float fl = null;
        try
        {
          fl = new Float( getOffsetInput().getText() );
          setValue( fl.floatValue() );
          fire();
        }
        catch( NumberFormatException nfe )
        {
          //TODO
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
              .getShell(), "Input needs to be of type float", "InputError-Stroke-Offset" );
          errorDialog.showError();
          getOffsetInput().setText( "" + getValue() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Label offsetLabel = new Label( composite, SWT.NULL );
    FormData offsetLabelData = new FormData();
    offsetLabelData.height = 15;
    offsetLabelData.width = 242;
    offsetLabelData.left = new FormAttachment( 0, 1000, 0 );
    offsetLabelData.top = new FormAttachment( 100, 1000, 0 );
    offsetLabel.setLayoutData( offsetLabelData );
    offsetLabel.setText( label );
  }

  public float getValue()
  {
    return value;
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

  public Text getOffsetInput()
  {
    return offsetInput;
  }

  public void setOffsetInput( Text m_offsetInput )
  {
    this.offsetInput = m_offsetInput;
  }

  public void setValue( float m_value )
  {
    this.value = m_value;
  }
}