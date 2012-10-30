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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.ui.internal.gml.WspmBuildingDecorator;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.IFigure;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.style.IAreaStyle;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner
 */
public class CulvertLayer extends AbstractProfilLayer
{
  private final ICulvertBuilding m_culvert;

  private EditInfo m_editInfo;

  private final IAreaStyle m_style;

  public CulvertLayer( final IProfile profil, final ICulvertBuilding culvert, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_TUBES, profil );

    m_culvert = culvert;

    m_style = styleProvider.getStyleFor( IWspmTuhhConstants.LAYER_TUBES + ILayerStyleProvider.AREA, IAreaStyle.class );
  }

  @Override
  public String getTitle( )
  {
    return m_culvert.getTypeLabel();
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final String description = m_culvert.getTypeLabel();
    final ImageDescriptor image = WspmBuildingDecorator.getBuildingImage( m_culvert );

    final ILegendEntry entry = new ImageLegendEntry( this, description, image );

    return new ILegendEntry[] { entry };
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    m_editInfo = null;

    final CulvertPainter painter = new CulvertPainter( m_culvert );

    final IFigure<IAreaStyle> tubeFigure = painter.createFigure( getCoordinateMapper() );
    if( tubeFigure == null )
      return;

    final IPaintable hoverFigure = tubeFigure;
    final String tooltip = "TUBE!";
    // TODO: create real geometry for hit-test

    m_editInfo = new EditInfo( this, hoverFigure, null, null, tooltip, null );

    tubeFigure.setStyle( m_style );
    tubeFigure.paint( gc );
  }

  @Override
  public IDataRange<Double> getDomainRange( )
  {
    return new CulvertPainter( m_culvert ).getDomainRange();
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange domainIntervall )
  {
    return new CulvertPainter( m_culvert ).getTargetRange();
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    return null;
  }

  @Override
  public void executeDrop( final Point point, final EditInfo dragStartData )
  {
  }

  @Override
  public void executeClick( final EditInfo dragStartData )
  {
  }

  @Override
  public EditInfo drag( final Point newPos, final EditInfo dragStartData )
  {
    return null;
  }

  @Override
  public EditInfo commitDrag( final Point point, final EditInfo dragStartData )
  {
    return null;
  }
}