/*
 * Created on 15.07.2004
 *  
 */
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
public class LabelPointPlacementPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  private Text xInput = null;

  private Text yInput = null;

  private double xValue = 0;

  private double yValue = 0;

  public LabelPointPlacementPanel( Composite parent, String m_label, double[] m_values )
  {
    setLabel( m_label );
    if( m_values != null && m_values.length >= 2 )
    {
      setXValue( m_values[0] );
      setYValue( m_values[1] );
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
    okButtonData.left = new FormAttachment( 810, 1000, 0 );
    okButtonData.top = new FormAttachment( 100, 1000, 0 );
    okButton.setLayoutData( okButtonData );
    okButton.setToolTipText( MessageBundle.STYLE_EDITOR_OK );

    yInput = new Text( composite, SWT.BORDER );
    FormData offsetInputData = new FormData();
    offsetInputData.height = 10;
    offsetInputData.width = 16;
    offsetInputData.left = new FormAttachment( 660, 1000, 0 );
    offsetInputData.top = new FormAttachment( 100, 1000, 0 );
    yInput.setLayoutData( offsetInputData );
    yInput.setText( "" + yValue );

    Label yLabel = new Label( composite, SWT.NULL );
    FormData yLabelData = new FormData();
    yLabelData.height = 16;
    yLabelData.width = 5;
    yLabelData.left = new FormAttachment( 640, 1000, 0 );
    yLabelData.top = new FormAttachment( 100, 1000, 0 );
    yLabel.setLayoutData( yLabelData );
    yLabel.setText( "/" );

    xInput = new Text( composite, SWT.BORDER );
    FormData xInputData = new FormData();
    xInputData.height = 10;
    xInputData.width = 16;
    xInputData.left = new FormAttachment( 490, 1000, 0 );
    xInputData.top = new FormAttachment( 100, 1000, 0 );
    xInput.setLayoutData( xInputData );
    xInput.setText( "" + xValue );

    final Label xLabel = new Label( composite, SWT.NULL );
    FormData xLabelData = new FormData();
    xLabelData.height = 15;
    xLabelData.width = 50;
    xLabelData.left = new FormAttachment( 340, 1000, 0 );
    xLabelData.top = new FormAttachment( 100, 1000, 0 );
    xLabel.setLayoutData( xLabelData );
    xLabel.setText( "x/y" );

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
        Double xDouble = null;
        Double yDouble = null;
        try
        {
          xDouble = new Double( getXInput().getText() );
          yDouble = new Double( getYInput().getText() );
          setXValue( xDouble.doubleValue() );
          if( getXValue() <= 0.0 )
          {
            setXValue( 1f );
            xLabel.setText( "" + getXValue() );
          }
          setYValue( yDouble.doubleValue() );
          fire();
        }
        catch( NumberFormatException nfe )
        {
          StyleEditorErrorDialog error = new StyleEditorErrorDialog( getComposite().getShell(),
              MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
              MessageBundle.STYLE_EDITOR_ERROR_NUMBER );
          error.showError();
          getXInput().setText( "" + getXValue() );
          getYInput().setText( "" + getYValue() );
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

  public double[] getValue()
  {
    double returnArray[] =
    { xValue, yValue };
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

  public Text getXInput()
  {
    return xInput;
  }

  public void setXInput( Text input )
  {
    xInput = input;
  }

  public double getXValue()
  {
    return xValue;
  }

  public void setXValue( double value )
  {
    xValue = value;
  }

  public Text getYInput()
  {
    return yInput;
  }

  public void setYInput( Text input )
  {
    yInput = input;
  }

  public double getYValue()
  {
    return yValue;
  }

  public void setYValue( double value )
  {
    yValue = value;
  }
}