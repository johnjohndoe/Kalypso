/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
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
    minLabel.setText( "min:" );

    minText = new Text( composite, SWT.BORDER );
    minText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData minTextData = new GridData( 15, 10 );
    minText.setLayoutData( minTextData );
    minText.setText( "" + min );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    Label maxLabel = new Label( composite, SWT.NULL );
    maxLabel.setText( "max:" );

    maxText = new Text( composite, SWT.BORDER );
    maxText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData maxTextData = new GridData( 15, 10 );
    maxText.setLayoutData( maxTextData );
    maxText.setText( "" + max );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    // null placeholder
    new Label( composite, SWT.NULL ).setLayoutData( new GridData( 50, 15 ) );

    Label stepLabel = new Label( composite, SWT.NULL );
    stepLabel.setText( "step:" );

    stepText = new Text( composite, SWT.BORDER );
    stepText.setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData stepTextData = new GridData( 15, 10 );
    stepText.setLayoutData( stepTextData );
    stepText.setText( "" + step );

    Button okButton = new Button( composite, SWT.PUSH );
    GridData okButtonData = new GridData( 22, 15 );
    okButton.setLayoutData( okButtonData );
    okButton.setText( "Ok" );
    okButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
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
                .getShell(), "Input value invalid", "Min<Max" );
            errorDialog.showError();
          }
          // step>(max-min)
          else if( t_step > ( t_max - t_min ) )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), "Input value invalid", "Step cannot be larger than (Max-Min)" );
            errorDialog.showError();
          }
          // step needs to be positive
          else if( t_step <= 0 )
          {
            StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog( getComposite()
                .getShell(), "Input value invalid", "Step needs to be positive >0" );
            errorDialog.showError();
          }
          // restrict editor to 50 steps
          else if( ( (int)Math.ceil( ( t_max - t_min ) / t_step ) ) > 50 )
          {
            new StyleEditorErrorDialog( getComposite().getShell(), "Verweis",
                "Editor ist auf 50 Abschnitte begrenzt" ).showError();
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
              .getShell(), "Input needs to be of type double", "needs to be double" );
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

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
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