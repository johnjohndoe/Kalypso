/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.core.profile;

import java.net.URL;
import java.util.HashMap;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.profil.IProfilePointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.util.StyleUtils;

/**
 * @author kimwerner
 */
public class DeviderProvider implements IProfilePointMarkerProvider
{
  private static final HashMap<String, RGB> MARKER_TYPES = new HashMap<>();

  public DeviderProvider( )
  {
    MARKER_TYPES.put( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, new RGB( 200, 50, 0 ) );
    MARKER_TYPES.put( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, new RGB( 0, 0, 255 ) );
    MARKER_TYPES.put( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, new RGB( 0, 180, 0 ) );
    MARKER_TYPES.put( IWspmTuhhConstants.MARKER_TYP_WEHR, new RGB( 0, 128, 0 ) );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getImageFor(java.lang.String)
   */
  @Override
  public void drawMarker( final String[] markers, final GC gc )
  {
    final int cnt = markers.length;
    final int offset = (gc.getClipping().width - 3 * cnt) / 2 + 1;
    int i = 0;
    final PolylineFigure rf = new PolylineFigure();

    rf.setStyle( StyleUtils.getDefaultStyle( LineStyle.class ) );
    rf.getStyle().setWidth( 3 );
    for( final String marker : markers )
    {
      final RGB rgb = MARKER_TYPES.get( marker );
      if( rgb != null )
      {

        rf.getStyle().setColor( rgb );

        rf.setPoints( new Point[] { new Point( offset + 4 * i, gc.getClipping().y ), new Point( offset + 4 * i++, gc.getClipping().height ) } );
        rf.paint( gc );
      }
    }
  }

  @Override
  public RGB getColorFor( final String marker )
  {
    return MARKER_TYPES.get( marker );
  }

  @Override
  public URL getSld( final String marker )
  {
    switch( marker )
    {
      case IWspmTuhhConstants.MARKER_TYP_BORDVOLL:
        return getClass().getResource( "marker/symbolization/bordvoll.point.sld" ); //$NON-NLS-1$
      case IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE:
        return getClass().getResource( "marker/symbolization/durstroemt.point.sld" ); //$NON-NLS-1$
      case IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE:
        return getClass().getResource( "marker/symbolization/trennflaeche.point.sld" ); //$NON-NLS-1$
      case IWspmTuhhConstants.MARKER_TYP_WEHR:
        return getClass().getResource( "marker/symbolization/wehr.sld" ); //$NON-NLS-1$
    }

    return null;
  }
}
