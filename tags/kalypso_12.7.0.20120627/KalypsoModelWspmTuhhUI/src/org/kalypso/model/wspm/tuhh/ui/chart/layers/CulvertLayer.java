/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.IFigure;
import de.openali.odysseus.chart.framework.model.style.IAreaStyle;
import de.openali.odysseus.chart.framework.model.style.IFill;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;

/**
 * @author kimwerner
 */
public class CulvertLayer extends AbstractProfilLayer
{
  public CulvertLayer( final IProfil profil, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_TUBES, profil, IWspmConstants.POINT_PROPERTY_HOEHE, styleProvider );
    getLineStyle().setColor( new RGB( 255, 255, 100 ) );
  }

  @Override
  public String getIdentifier( )
  {
    return getTube() == null ? Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.TubeLayer.0" ) : getTube().getObservation().getName(); //$NON-NLS-1$
  }

  private IProfileBuilding getTube( )
  {
    final IProfileBuilding[] objects = getProfil().getProfileObjects( IProfileBuilding.class );
    if( ArrayUtils.isEmpty( objects ) )
      return null;

    return objects[0];
  }

  @Override
  public String getTitle( )
  {
    return getTube() == null ? "" : getTube().getObservation().getDescription(); //$NON-NLS-1$
  }

  @Override
  public void paint( final GC gc )
  {
    final IProfileBuilding tube = getTube();
    if( tube == null )
      return;

    final CulvertPainter painter = new CulvertPainter( tube );

    final IFigure<IAreaStyle> tubeFigure = painter.createFigure( getCoordinateMapper() );

    if( tubeFigure == null )
      return;

    final IAreaStyle areaStyle = createStyle();
    tubeFigure.setStyle( areaStyle );
    tubeFigure.paint( gc );
  }

  private IAreaStyle createStyle( )
  {
    final IFill fill = new ColorFill( new RGB( 255, 255, 100 ) );
    final ILineStyle stroke = new LineStyle( 2, new RGB( 0, 0, 0 ), 255, 0f, null, LINEJOIN.ROUND, LINECAP.ROUND, 1, true );

    return new AreaStyle( fill, 128, stroke, true );
  }

  @Override
  public IDataRange<Number> getDomainRange( )
  {
    return new CulvertPainter( getTube() ).getDomainRange();
  }

  @Override
  public IDataRange< ? > getTargetRange( final IDataRange< ? > domainIntervall )
  {
    return new CulvertPainter( getTube() ).getTargetRange();
  }
}