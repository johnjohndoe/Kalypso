package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Text;

/**
 * @author F.Lindemann
 *  
 */
public class SliderPanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private int min = 0;

  private int max = 100;

  private int increment = 0;

  private Text text = null;

  private Slider slider = null;

  public final static int DECIMAL = 0;

  public final static int INTEGER = 1;

  private int format = 1;

  private String label = null;

  private double selection = 0.0;

  public SliderPanel( Composite parent, String m_label, int minimum, int maximum, int inc,
      int m_format, double value )
  {
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
    text = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    slider = new Slider( composite, SWT.HORIZONTAL );

    setLabel( m_label );
    setFormat( m_format );
    this.min = minimum * 100;
    this.max = ( maximum - minimum ) * 100;
    if( inc >= maximum )
      this.increment = max / 10;
    else
      this.increment = inc * 100;
    init();

    if( value != -1 && value <= maximum )
      setSelection( value - minimum );
    else if( value > maximum )
      setSelection( maximum );
  }

  public void setSelection( double m_selection )
  {
    this.selection = m_selection;
    if( format == DECIMAL )
      text.setText( "" + ( selection + ( min / 100.00 ) ) );
    else
      text.setText( "" + (int)( selection + ( min / 100.00 ) ) );
    slider.setSelection( (int)( selection * 100 ) );
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    text.setBackground( new Color( null, new RGB( 255, 255, 255 ) ) );

    FormData textData = new FormData();
    textData.height = 10;
    textData.width = 20;
    textData.left = new FormAttachment( 340, 1000, 0 );
    textData.top = new FormAttachment( 120, 1000, 0 );
    text.setLayoutData( textData );
    FormData sliderData = new FormData();
    sliderData.height = 17;
    sliderData.width = 90;
    sliderData.left = new FormAttachment( 540, 1000, 0 );
    sliderData.top = new FormAttachment( 100, 1000, 0 );
    slider.setLayoutData( sliderData );
    slider.setIncrement( increment );
    slider.setMaximum( max + slider.getThumb() );
    slider.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {/**/
      }

      public void mouseDown( MouseEvent e )
      { /**/
      }

      public void mouseUp( MouseEvent e )
      {
        setSelection( ( getSlider().getSelection() ) / 100.00 );
        fire();
      }
    } );
    slider.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        if( getFormat() == INTEGER )
          setSelection( ( ( (Slider)e.getSource() ).getSelection() ) / 100 );
        else
          setSelection( ( ( (Slider)e.getSource() ).getSelection() ) / 100.0 );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    Label fillColorLabel = new Label( composite, SWT.NULL );
    FormData fillColorLabelLData = new FormData();
    fillColorLabelLData.height = 15;
    fillColorLabelLData.width = 242;
    fillColorLabelLData.left = new FormAttachment( 0, 1000, 0 );
    fillColorLabelLData.top = new FormAttachment( 100, 1000, 0 );
    fillColorLabel.setLayoutData( fillColorLabelLData );
    fillColorLabel.setText( label );
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

  public double getSelection()
  {
    if( format == DECIMAL )
      return ( selection + ( min / 100.00 ) );
    return (int)( selection + ( min / 100.00 ) );
  }

  public int getFormat()
  {
    return format;
  }

  public void setFormat( int m_format )
  {
    this.format = m_format;
  }

  public int getIncrement()
  {
    return increment;
  }

  public void setIncrement( int m_increment )
  {
    this.increment = m_increment;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public int getMax()
  {
    return max;
  }

  public void setMax( int m_max )
  {
    this.max = m_max;
  }

  public int getMin()
  {
    return min;
  }

  public void setMin( int m_min )
  {
    this.min = m_min;
  }

  public Slider getSlider()
  {
    return slider;
  }

  public void setSlider( Slider m_slider )
  {
    this.slider = m_slider;
  }
}