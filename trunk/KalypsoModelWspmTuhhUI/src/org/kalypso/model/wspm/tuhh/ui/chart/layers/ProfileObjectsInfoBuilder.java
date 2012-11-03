/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.ProfileStrings;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.ext.base.layer.TooltipFormatter;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;

/**
 * Default implementation for arbitrary profile objects.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectsInfoBuilder implements IProfileObjectInfoBuilder
{
  private final IProfilChartLayer m_layer;

  private final PartTypeAccessor m_partInfo;

  public ProfileObjectsInfoBuilder( final IProfilChartLayer layer, final PartTypeAccessor partInfo )
  {
    m_layer = layer;
    m_partInfo = partInfo;
  }

  protected PartTypeAccessor getPartInfo( )
  {
    return m_partInfo;
  }

  @Override
  public EditInfo createPointInfo( final IProfileObject object, final IProfileObjectRecord record, final PointFigure hoverFigure, final Point point )
  {
    final String tooltip = formatPointTooltip( object, record );

    return new EditInfo( m_layer, hoverFigure, null, record, tooltip, point );
  }

  protected String formatPointTooltip( final IProfileObject object, final IProfileObjectRecord record )
  {
    final String pointHeader = getPointHeader( object, record );

    final TooltipFormatter formatter = new TooltipFormatter( pointHeader, new String[] { "%s", "%s", "%s" }, new int[] { SWT.LEFT, SWT.RIGHT, SWT.LEFT } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // TODO: get labels and units from profile object
    formatter.addLine( ProfileStrings.PROFILE_OBJECT_RECORD_WIDTH_LABEL, String.format( "%,.2f", record.getBreite() ), ProfileStrings.PROFILE_OBJECT_RECORD_WIDTH_UNIT ); //$NON-NLS-2$
    formatter.addLine( ProfileStrings.PROFILE_OBJECT_RECORD_HEIGHT_LABEL, String.format( "%,.2f", record.getHoehe() ), ProfileStrings.PROFILE_OBJECT_RECORD_HEIGHT_UNIT ); //$NON-NLS-2$

    final String code = record.getCode();
    if( !StringUtils.isBlank( code ) )
      formatter.addLine( ProfileStrings.PROFILE_OBJECT_RECORD_CODE_LABEL, code, StringUtils.EMPTY );

    final String comment = record.getComment();
    if( !StringUtils.isBlank( comment ) )
      formatter.addFooter( comment );

    return formatter.format();
  }

  protected String getPointHeader( @SuppressWarnings( "unused" ) final IProfileObject object, @SuppressWarnings( "unused" ) final IProfileObjectRecord record )
  {
    return m_partInfo.getTypeLabel();
  }

  @Override
  public EditInfo createLineInfo( final IProfileObject object, final PolylineFigure hoverFigure )
  {
    final String tooltip = formatLineTooltip( object );

    return new EditInfo( m_layer, hoverFigure, null, object, tooltip, null );
  }

  protected String formatLineTooltip( final IProfileObject object )
  {
    final String typeLabel = m_partInfo.getTypeLabel();

    final String description = object.getDescription();

    if( StringUtils.isBlank( description ) )
      return typeLabel;

    return String.format( "%s - %s", typeLabel, description ); //$NON-NLS-1$
  }
}