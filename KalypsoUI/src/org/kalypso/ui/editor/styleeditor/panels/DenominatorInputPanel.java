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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;

/**
 * @author F.Lindemann
 *  
 */
public class DenominatorInputPanel
{

  private Composite composite = null;

  private double denominator = 0.0;

  private Text text = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  public DenominatorInputPanel( Composite parent, String m_label, double m_denominator )
  {
    setLabel( m_label );
    setDenominator( m_denominator );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 225;
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
    setText( new Text( composite, SWT.BORDER ) );
    text.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );

    FormData textData = new FormData();
    textData.height = 10;
    textData.width = 90;
    textData.left = new FormAttachment( 295, 1000, 0 );
    textData.top = new FormAttachment( 10, 1000, 0 );
    text.setLayoutData( textData );
    text.setText( "" + denominator );

    Label okButton = new Label( composite, SWT.PUSH );
    okButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_OK.createImage() );
    FormData okButtonData = new FormData();
    okButtonData.height = 15;
    okButtonData.width = 22;
    okButtonData.left = new FormAttachment( 900, 1000, 0 );
    okButtonData.top = new FormAttachment( 100, 1000, 0 );
    okButton.setLayoutData( okButtonData );
    okButton.setToolTipText( MessageBundle.STYLE_EDITOR_OK );
    okButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        try
        {
          setDenominator( Double.parseDouble( getText().getText() ) );
          if( getDenominator() < 0 )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
                MessageBundle.STYLE_EDITOR_ERROR_POSITIVE );
            errorDialog.showError();
          }
          else
            fire();
        }
        catch( NumberFormatException nfe )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
              .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
              MessageBundle.STYLE_EDITOR_ERROR_NUMBER );
          errorDialog.showError();
          getText().setText( "" + getDenominator() );
        }
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );

    Label getCurrentScaleButton = new Label( composite, SWT.PUSH );
    getCurrentScaleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_GET_SCALE.createImage() );
    FormData getCurrentScaleButtonData = new FormData();
    getCurrentScaleButtonData.height = 15;
    getCurrentScaleButtonData.width = 22;
    getCurrentScaleButtonData.left = new FormAttachment( 770, 1000, 0 );
    getCurrentScaleButtonData.top = new FormAttachment( 100, 1000, 0 );
    getCurrentScaleButton.setLayoutData( getCurrentScaleButtonData );
    getCurrentScaleButton.setToolTipText( MessageBundle.STYLE_EDITOR_SCALE );

    getCurrentScaleButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
        IEditorPart editor = window.getActivePage().getActiveEditor();
        if( editor instanceof GisMapEditor )
        {
          GisMapEditor gisMapEditor = (GisMapEditor)editor;
          setDenominator( gisMapEditor.getMapPanel().getCurrentScale() );
        }
        getText().setText( "" + getDenominator() );
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

  public double getDenominator()
  {
    return denominator;
  }

  // sets the inputField to a default state
  public void reset()
  {
    text.setText( "" );
  }

  public void setDenominator( double denom )
  {
    this.denominator = denom;
    if( text != null && !text.isDisposed() )
      text.setText( "" + denominator );
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
}