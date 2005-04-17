/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.mapmodel;

import java.awt.Graphics;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class MapModell implements IMapModell
{
  private final ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private final static Boolean THEME_ENABLED = Boolean.valueOf( true );

  private final static Boolean THEME_DISABLED = Boolean.valueOf( false );

  private final Vector myThemes = new Vector();

  private final Map myEnabledThemeStatus = new HashMap();

  private final CS_CoordinateSystem myCoordinatesSystem;

  private IKalypsoTheme m_activeTheme = null;

  public MapModell( final CS_CoordinateSystem crs )
  {
    myCoordinatesSystem = crs;
  }

  public void dispose()
  {
    for( Iterator iter = myThemes.iterator(); iter.hasNext(); )
      ( (IKalypsoTheme)iter.next() ).dispose();

    myThemes.clear();
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    // TODO: check, ob thema �berhaupt hier vorhanden?
    m_activeTheme = theme;
    fireModellEvent( null );
  }

  public IKalypsoTheme getActiveTheme()
  {
    return m_activeTheme;
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    if( m_activeTheme == null )
      m_activeTheme = theme;

    myThemes.add( theme );

    myEnabledThemeStatus.put( theme, THEME_ENABLED );

    theme.addModellListener( this );

    fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );
  }

  public void clear()
  {
    m_activeTheme = null;
    IKalypsoTheme[] themes = getAllThemes();
    for( int i = 0; i < themes.length; i++ )
      removeTheme( themes[i] );
    fireModellEvent( null );
  }

  public void enableTheme( final IKalypsoTheme theme, final boolean status )
  {
    // TODO: check if theme is in this model?
    if( status )
      myEnabledThemeStatus.put( theme, THEME_ENABLED );
    else
      myEnabledThemeStatus.put( theme, THEME_DISABLED );
    fireModellEvent( null );
  }

  public IKalypsoTheme[] getAllThemes()
  {
    return (IKalypsoTheme[])myThemes.toArray( new IKalypsoTheme[myThemes.size()] );
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCoordinatesSystem;
  }

  //  /**
  //   * renders the map to the passed graphic context
  //   *
  //   * @param g
  //   * @throws RenderException
  //   * thrown if the passed <tt>Graphic<tt> haven't
  //   * clipbounds. use g.setClip( .. );
  //   */
  //  public void paint( final Graphics g ) throws RenderException
  //  {
  //    if( getThemeSize() == 0 )
  //      return;
  //    if( g.getClipBounds() == null )
  //    {
  //      throw new RenderException( "no clip bounds defined for graphic context" );
  //    }
  //
  //    int x = g.getClipBounds().x;
  //    int y = g.getClipBounds().y;
  //    int w = g.getClipBounds().width;
  //    int h = g.getClipBounds().height;
  //    myProjection.setDestRect( x - 2, y - 2, w + x, h + y );
  //    
  //    //myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
  //    
  //    final double scale = calcScale( g.getClipBounds().width,
  // g.getClipBounds().height );
  //    final GeoTransform p = getProjection();
  //    final GM_Envelope bbox = getBoundingBox();
  //    
  //    for( int i = 0; i < getThemeSize(); i++ )
  //    {
  //      if( isThemeEnabled( getTheme( i ) ) )
  //        getTheme( i ).paint( g, p, scale, bbox );
  //    }
  //  }

  public void paintSelected( final Graphics g, final GeoTransform p, final GM_Envelope bbox,
      final double scale, final int selectionId )
  {
    if( getThemeSize() == 0 )
      return;

    for( int i = 0; i < getThemeSize(); i++ )
    {
      IKalypsoTheme theme = getTheme( getThemeSize() - i - 1 );
      if( isThemeEnabled( theme ) )
        theme.paintSelected( g, p, scale, bbox, selectionId );
    }
  }

  //  public double getScale()
  //  {
  //    return myScale;
  //  }
  //
  //  public double getScale( Graphics g )
  //  {
  //    myScale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
  //    return myScale;
  //  }

  public IKalypsoTheme getTheme( int pos )
  {
    return (IKalypsoTheme)myThemes.elementAt( pos );
  }

  public IKalypsoTheme getTheme( String themeName )
  {
    for( int i = 0; i < myThemes.size(); i++ )
      if( themeName.equals( ( (IKalypsoTheme)myThemes.elementAt( i ) ).getName() ) )
        return (IKalypsoTheme)myThemes.elementAt( i );
    return null;
  }

  public int getThemeSize()
  {
    return myThemes.size();
  }

  public boolean isThemeActivated( IKalypsoTheme theme )
  {
    return m_activeTheme == theme;
  }

  public boolean isThemeEnabled( IKalypsoTheme theme )
  {
    return myEnabledThemeStatus.get( theme ) == THEME_ENABLED;
  }

  public void moveDown( IKalypsoTheme theme )
  {
    int pos = myThemes.indexOf( theme );
    if( pos > 0 )
      swapThemes( theme, getTheme( pos - 1 ) );
  }

  public void moveUp( IKalypsoTheme theme )
  {
    int pos = myThemes.indexOf( theme );
    if( pos + 1 < myThemes.size() )
      swapThemes( theme, getTheme( pos + 1 ) );
  }

  public void removeTheme( int pos )
  {
    removeTheme( (IKalypsoTheme)myThemes.elementAt( pos ) );
  }

  public void removeTheme( String themeName )
  {
    removeTheme( getTheme( themeName ) );
  }

  public void removeTheme( IKalypsoTheme theme )
  {
    myThemes.remove( theme );
    myEnabledThemeStatus.remove( theme );
    if( m_activeTheme == theme )
      m_activeTheme = null;
    fireModellEvent( null );
  }

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    if( crs.equals( myCoordinatesSystem ) )
      throw new UnsupportedOperationException();
  }

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 )
  {
    int pos1 = myThemes.indexOf( theme1 );
    int pos2 = myThemes.indexOf( theme2 );
    myThemes.set( pos1, theme2 );
    myThemes.set( pos2, theme1 );
    fireModellEvent( null );
  }

  public GM_Envelope getFullExtentBoundingBox()
  {
    final IKalypsoTheme[] themes = getAllThemes();
    GM_Envelope result = null;
    for( int i = 0; i < themes.length; i++ )
    {
      if( isThemeEnabled( themes[i] ) )
      {
        try
        {
          final GM_Envelope boundingBox = themes[i].getBoundingBox();

          if( result == null )
            result = boundingBox;
          else
            result = result.getMerged( boundingBox );
        }
        catch( final Exception e )
        {
          // TODO: das sollte nicht sein, exception einfach weiterwerfen
          e.printStackTrace();
        }
      }
    }
    return result;
  }

  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }
}