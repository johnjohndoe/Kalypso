/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import org.kalypso.ogc.MapPanel;

/**
 * Der Controller fuer die MapView
 * 
 * @author vdoemming
 */
public class WidgetManager implements MouseListener, MouseMotionListener
{
  private IWidget myNormalWidget = null;

  private IWidget myTemporaryWidget = null;

  private final MapPanel myMapPanel;

  public WidgetManager( final MapPanel mapPanel )
  {
    myMapPanel = mapPanel;
  }

  // MouseAdapter
  public void mouseClicked( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;

    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
      case MouseEvent.BUTTON1:
        actualWidget.leftClicked( e.getPoint() );
        break;

      case MouseEvent.BUTTON2:
        actualWidget.middleClicked( e.getPoint() );
        break;

      case MouseEvent.BUTTON3:
        actualWidget.rightClicked( e.getPoint() );
        stopTemporaryWidget();
        break;

      default:
        break;
      }
  }

  // MouseMotionAdapter:
  public void mouseDragged( MouseEvent e )
  {
    if( getActualWidget() != null )
    {
      getActualWidget().dragged( e.getPoint() );

      myMapPanel.repaint();
    }

  }

  public void mouseEntered( MouseEvent e )
  {
  //
  }

  public void mouseExited( MouseEvent e )
  {
  //
  }

  public void mouseMoved( MouseEvent e )
  {
    if( getActualWidget() != null )
    {
      getActualWidget().moved( e.getPoint() );

      myMapPanel.repaint();
    }

  }

  public void mousePressed( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( actualWidget == null )
      return;
    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
      case MouseEvent.BUTTON1:
        actualWidget.leftPressed( e.getPoint() );

        break;

      case MouseEvent.BUTTON2:
        actualWidget.middlePressed( e.getPoint() );

        break;

      case MouseEvent.BUTTON3:
        actualWidget.rightPressed( e.getPoint() );

        break;

      default:
        break;
      }
  }

  public void mouseReleased( MouseEvent e )
  {
    final IWidget actualWidget = getActualWidget();
    if( getActualWidget() == null )
      return;

    if( e.isPopupTrigger() )
      actualWidget.clickPopup( e.getPoint() );
    else
      switch( e.getButton() )
      {
      case MouseEvent.BUTTON1: // Left
        actualWidget.leftReleased( e.getPoint() );

        break;

      case MouseEvent.BUTTON2:
        actualWidget.middleReleased( e.getPoint() );

        break;

      case MouseEvent.BUTTON3: //Right
        actualWidget.perform();

        //		    getActualWidget().rightReleased(e.getPoint());
        break;

      default:
        break;
      }

  }

  public void paintWidget( Graphics g )
  {
    if( getActualWidget() != null )
    {
      getActualWidget().paint( g );
    }
  }

  public IWidget getActualWidget()
  {
    if( myTemporaryWidget != null )
      return myTemporaryWidget;

    return myNormalWidget;
  }

  //    private Widget getFeatureCreateWidget( )
  //    {
  //        FeatureType ft = null;
  //        Theme activeTheme = JMThemes.getInstance( ).getActiveTheme( );
  //        FeatureType[] fts = JMFeatureTypeManager.getInstance( ).getFeatureTypes(
  // activeTheme );
  //
  //        /* if(fts.length<2)
  //            ft=fts[0];
  //        else
  //        */
  //        {
  //            ft = Tools.askFeatureType( fts );
  //        }
  //
  //        FeatureTypeProperty[] ftps = ft.getProperties( );
  //        FeatureTypeProperty ftp = null;
  //
  //        for( int p = 0; p < ftps.length; p++ )
  //        {
  //            System.out.println( ftps[p].getType( ) );
  //
  //            if( ftps[p].getType( ).startsWith( "org.deegree.model.geometry." ) )
  //            {
  //                if( ftps[p].getType( ).equals( "org.deegree.model.geometry.GM_Point" ) )
  //                    return new CreateFeatureWithPointWidget( activeTheme, ft, ftps[p] );
  //
  //                if( ftps[p].getType( ).equals( "org.deegree.model.geometry.GM_LineString" )
  // )
  //                    return new CreateFeatureWithLineStringWidget( activeTheme, ft, ftps[p] );
  //
  //                if( ftps[p].getType( ).equals( "org.deegree.model.geometry.GM_Polygon" ) )
  //                    return new CreateFeatureWithPolygonWidget( activeTheme, ft, ftps[p] );
  //            }
  //        }
  //
  //        return new FeatureEditWidget( );
  //    }

  public void changeWidget( final IWidget newWidget )
  {
    if( newWidget == null )
    {
      myNormalWidget = null;
      return;
    }
    if( myTemporaryWidget != null ) // finish temporary widget if required
    {
      myTemporaryWidget.finish();
      myTemporaryWidget = null;
    }

    if( newWidget instanceof TemporaryActionWidget )
    {
      myTemporaryWidget = newWidget;
    }
    else
    // normal widget
    {
      if( myNormalWidget != null )// && normalWidget != newWidget )
        myNormalWidget.finish();

      myNormalWidget = newWidget;
      myNormalWidget.activate( myMapPanel );
    }

    if( getActualWidget() != null )
    {
      //            JPanel panel = new JPanel( );
      //            panel.setLayout( new BorderLayout( ) );
      //
      //            if( normalWidget != null && normalWidget != getActualWidget( ) )
      //            {
      //                JButton lastWidgetButton = new JButton( normalWidget.getName( ) );
      //                lastWidgetButton.setActionCommand( "stopTemporaryWidget" );
      //                lastWidgetButton.addActionListener( this );
      //                panel.add( lastWidgetButton, BorderLayout.SOUTH );
      //            }
      //
      //            panel.add( new JLabel( getActualWidget( ).getName( ) ),
      // BorderLayout.NORTH );
      //
      //            JComponent component = getActualWidget( ).getOptionDialog( );
      //
      //            if( component != null )
      //                panel.add( component, BorderLayout.CENTER );
      //
      //            JMMapFrame.getInstance( ).setOptionDialog( panel );
      //       
    }
  }

  private void stopTemporaryWidget()
  {
    if( myTemporaryWidget != null )
    {
      myTemporaryWidget.finish();
      myTemporaryWidget = null;
    }

    if( getActualWidget() != null )
      changeWidget( getActualWidget() );
  }
}