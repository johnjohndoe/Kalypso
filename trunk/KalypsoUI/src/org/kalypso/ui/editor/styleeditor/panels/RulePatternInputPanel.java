/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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
public class RulePatternInputPanel
{

  private Composite composite = null;

  private Text minText = null;

  private Text maxText = null;

  private Text stepText = null;

  private double min;

  private double max;

  private double step;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  public RulePatternInputPanel( Composite parent, String m_label, double m_min, double m_max,
      double m_step )
  {
    setLabel( m_label );
    setMin( m_min );
    setMax( m_max );
    setStep( m_step );
    composite = new Composite( parent, SWT.NULL );
    GridLayout compositeLayout = new GridLayout( 4, false );
    GridData compositeData = new GridData();
    compositeData.widthHint = 225;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    composite.layout();
    init();
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    Label urlLabel = new Label( composite, SWT.NULL );
    GridData urlLabelData = new GridData( 62, 15 );
    urlLabel.setLayoutData( urlLabelData );
    urlLabel.setText( label );

    Label minLabel = new Label( composite, SWT.NULL );
    minLabel.setText( MessageBundle.STYLE_EDITOR_MIN );

    minText = new Text( composite, SWT.BORDER );
    minText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData minTextData = new GridData( 25, 10 );
    minText.setLayoutData( minTextData );
    minText.setText( "" + min );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    Label maxLabel = new Label( composite, SWT.NULL );
    maxLabel.setText( MessageBundle.STYLE_EDITOR_MAX );

    maxText = new Text( composite, SWT.BORDER );
    maxText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData maxTextData = new GridData( 25, 10 );
    maxText.setLayoutData( maxTextData );
    maxText.setText( "" + max );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    Label stepLabel = new Label( composite, SWT.NULL );
    stepLabel.setText( MessageBundle.STYLE_EDITOR_STEP );

    stepText = new Text( composite, SWT.BORDER );
    stepText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData stepTextData = new GridData( 25, 10 );
    stepText.setLayoutData( stepTextData );
    stepText.setText( "" + step );

    Label okButton = new Label( composite, SWT.PUSH );
    okButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_OK.createImage() );
    GridData okButtonData = new GridData( 22, 15 );
    okButton.setLayoutData( okButtonData );
    okButton.setToolTipText( MessageBundle.STYLE_EDITOR_OK );
    okButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {
        try
        {
          double t_min = Double.parseDouble( getMinText().getText() );
          double t_max = Double.parseDouble( getMaxText().getText() );
          double t_step = Double.parseDouble( getStepText().getText() );
          // check input
          // 1. min < max !!!
          if( t_min > t_max )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
                MessageBundle.STYLE_EDITOR_MIN_MAX );
            errorDialog.showError();
          }
          // step>(max-min)
          else if( t_step > ( t_max - t_min ) )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
                MessageBundle.STYLE_EDITOR_STEP_TOO_LARGE );
            errorDialog.showError();
          }
          // step needs to be positive
          else if( t_step <= 0 )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
                MessageBundle.STYLE_EDITOR_STEP_NOT_POSITIVE );
            errorDialog.showError();
          }
          // restrict editor to 35 steps
          else if( ( (int)Math.ceil( ( t_max - t_min ) / t_step ) ) > 35 )
          {
            new StyleEditorErrorDialog( getComposite().getShell(),
                MessageBundle.STYLE_EDITOR_REMARK, MessageBundle.STYLE_EDITOR_PATTERN_LIMIT )
                .showError();
          }
          else
          {
            setMin( t_min );
            setMax( t_max );
            setStep( t_step );
            fire();
          }
        }
        catch( NumberFormatException nfe )
        {
          StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
              .getShell(), MessageBundle.STYLE_EDITOR_ERROR_INVALID_INPUT,
              MessageBundle.STYLE_EDITOR_ERROR_NUMBER );
          errorDialog.showError();
          getMinText().setText( "" + getMin() );
          getMaxText().setText( "" + getMax() );
          getStepText().setText( "" + getStep() );
        }
        catch( Exception ex )
        {
          ex.printStackTrace();
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

  public double getMax()
  {
    return max;
  }

  public double getMin()
  {
    return min;
  }

  public double getStep()
  {
    return step;
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

  public Text getMaxText()
  {
    return maxText;
  }

  public void setMaxText( Text m_maxText )
  {
    this.maxText = m_maxText;
  }

  public Text getMinText()
  {
    return minText;
  }

  public void setMinText( Text m_minText )
  {
    this.minText = m_minText;
  }

  public Text getStepText()
  {
    return stepText;
  }

  public void setStepText( Text m_stepText )
  {
    this.stepText = m_stepText;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public void setMax( double m_max )
  {
    this.max = m_max;
  }

  public void setMin( double m_min )
  {
    this.min = m_min;
  }

  public void setStep( double m_step )
  {
    this.step = m_step;
  }
}