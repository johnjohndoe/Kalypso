/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * @author Administrator
 *  
 */
public class ColorPalettePanel
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  public final static int CUSTOM_TRANSITION = -1;

  public final static int RED_GREEN_TRANSITION = 0;

  public final static int BLUE_GREEN_TRANSITION = 1;

  public final static int RED_BLUE_TRANSITION = 2;

  private int numberOfColors = 0;

  private final int COLOR_SIZE = 10;

  private final int COLOR_BORDER = 2;

  public int type = RED_GREEN_TRANSITION;

  private Color[] customColor = null;

  private Color[] colorArray = null;

  private int colorPaletteSelection = 0;

  public ColorPalettePanel( Composite parent, Color[] colors, int m_numberOfColors )
  {
    this.type = RED_GREEN_TRANSITION;
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 180;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    if( colors == null )
      initializeColors( type, m_numberOfColors );
    else
      setColorArray( colors );
    setNumberOfColors( getColorArray().length );
    init();
    customColor = getColorArray();
  }

  public void addColorPalettePanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  public void draw( Composite parent )
  {
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 180;
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
    Composite palleteParentComposite = new Composite( composite, SWT.NULL );
    palleteParentComposite.setLayout( new GridLayout() );
    FormData palleteParentCompositeData = new FormData();
    palleteParentCompositeData.left = new FormAttachment( 340, 1000, 0 );
    palleteParentCompositeData.top = new FormAttachment( 0, 1000, 0 );
    palleteParentComposite.setLayoutData( palleteParentCompositeData );

    final ColorPaletteComboBox comboBox = new ColorPaletteComboBox( palleteParentComposite );
    comboBox.setSelection( colorPaletteSelection );
    final ColorPalette colorPallete = new ColorPalette( palleteParentComposite, getColorArray(),
        COLOR_SIZE, COLOR_BORDER );

    comboBox.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        try
        {
          switch( comboBox.getSelection() )
          {
          case 0:
          {
            initializeColors( RED_GREEN_TRANSITION, getNumberOfColors() );
            break;
          }
          case 1:
          {
            initializeColors( BLUE_GREEN_TRANSITION, getNumberOfColors() );
            break;
          }
          case 2:
          {
            initializeColors( RED_BLUE_TRANSITION, getNumberOfColors() );
            break;
          }
          case 3:
          {
            initializeColors( CUSTOM_TRANSITION, getNumberOfColors() );
            break;
          }
          default:
          {
            initializeColors( RED_GREEN_TRANSITION, getNumberOfColors() );
          }
          }
          colorPallete.setColors( getColorArray() );
          setColorPaletteSelection( comboBox.getSelection() );
          fire();
        }
        catch( Exception exx )
        {
          exx.printStackTrace();
        }
      }
    } );

    colorPallete.addColorPaletterListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        // changes allowed only to customize selection
        setColorPaletteSelection( 3 );
        setType( CUSTOM_TRANSITION );
        setCustomColor( colorPallete.getColors() );
        setColorArray( colorPallete.getColors() );
        fire();
      }
    } );

    Label fillColorLabel = new Label( composite, SWT.NULL );
    FormData fillColorLabelLData = new FormData();
    fillColorLabelLData.height = 15;
    fillColorLabelLData.width = 242;
    fillColorLabelLData.left = new FormAttachment( 0, 1000, 0 );
    fillColorLabelLData.top = new FormAttachment( 100, 1000, 0 );
    fillColorLabel.setLayoutData( fillColorLabelLData );
    fillColorLabel.setText( "test" );
  }

  //public static Color[] initializeColors(int type,int numberOfColors)
  public void initializeColors( int m_type, int m_numberOfColors )
  {
    this.type = m_type;
    if( type == CUSTOM_TRANSITION )
    {
      initializeCustomColors( m_numberOfColors );
      return;
    }
    Color[] colors = new Color[m_numberOfColors];
    int step = 255 / m_numberOfColors;
    for( int i = 0; i < m_numberOfColors; i++ )
    {
      if( type == BLUE_GREEN_TRANSITION )
      {
        colors[i] = new Color( null, 0, i * step, 255 - ( i * step ) );
      }
      else if( type == RED_BLUE_TRANSITION )
      {
        colors[i] = new Color( null, 255 - ( i * step ), 0, i * step );
      }
      else if( type == RED_GREEN_TRANSITION )
      {
        colors[i] = new Color( null, 255 - ( i * step ), i * step, 0 );
      }
    }
    setColorArray( colors );
  }

  private void initializeCustomColors( int m_numberOfColors )
  {
    // check whether size is appropriate
    if( customColor.length < m_numberOfColors )
    {
      Color[] tmpColor = new Color[m_numberOfColors];
      int i = 0;
      for( ; i < customColor.length; i++ )
        tmpColor[i] = customColor[i];
      for( ; i < m_numberOfColors; i++ )
        tmpColor[i] = new Color( null, 0, 0, 0 );
      customColor = tmpColor;
    }
    else if( customColor.length > m_numberOfColors )
    {
      Color[] tmpColor = new Color[m_numberOfColors];
      for( int i = 0; i < m_numberOfColors; i++ )
        tmpColor[i] = customColor[i];
      customColor = tmpColor;
    }
    setColorArray( customColor );
  }

  public void setColorPalette( Color[] colors )
  {
    setColorArray( colors );
  }

  public Color[] getColorPalette()
  {
    return getColorArray();
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

  public Color[] getColorArray()
  {
    return colorArray;
  }

  public void setColorArray( Color[] m_colorArray )
  {
    numberOfColors = m_colorArray.length;
    this.colorArray = m_colorArray;
  }

  public int getType()
  {
    return type;
  }

  public void setType( int m_type )
  {
    this.type = m_type;
  }

  public int getNumberOfColors()
  {
    return numberOfColors;
  }

  public void setNumberOfColors( int m_numberOfColors )
  {
    this.numberOfColors = m_numberOfColors;
  }

  public int getColorPaletteSelection()
  {
    return colorPaletteSelection;
  }

  public void setColorPaletteSelection( int m_colorPaletteSelection )
  {
    this.colorPaletteSelection = m_colorPaletteSelection;
  }

  public Color[] getCustomColor()
  {
    return customColor;
  }

  public void setCustomColor( Color[] m_customColor )
  {
    this.customColor = m_customColor;
  }
}