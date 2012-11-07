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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;

import de.openali.odysseus.chart.ext.base.layer.HoverIndex;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * Layer that renders the records of a {@link org.kalypso.model.wspm.core.profil.IProfileObject}.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectsLayer extends AbstractProfilLayer
{
  private final IProfileObject m_object;

  private final ProfileObjectPainter m_painter;

  private HoverIndex m_hoverIndex;

  public ProfileObjectsLayer( final String id, final IProfile profile, final IProfileObject object, final String title )
  {
    super( id, profile );

    m_object = object;

    final String type = object.getType();

    final PartTypeAccessor partInfo = new PartTypeAccessor( profile, type );
    final ProfileObjectsInfoBuilder infoBuilder = new ProfileObjectsInfoBuilder( this, partInfo );

    m_painter = new ProfileObjectPainter( partInfo, infoBuilder );

    final String label = partInfo.getTypeLabel();

    //final String typeTitle = String.format( "%s", label, m_object.getDescription() ); //$NON-NLS-1$
    final String typeTitle = label;

    if( title == null )
      setTitle( typeTitle );
    else
      setTitle( title );
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final ILegendEntry entry = new LegendEntry( this, getTitle() )
    {
      @Override
      public void paintSymbol( final GC gc, final Point size )
      {
        paintLegendSymbol( gc, size );
      }
    };

    return new ILegendEntry[] { entry };
  }

  protected void paintLegendSymbol( final GC gc, final Point size )
  {
    m_painter.paintLegend( gc, size );
  }

  @Override
  public IDataRange<Double> getDomainRange( )
  {
    return m_painter.getDomainRange( m_object );
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange domainRange )
  {
    return m_painter.getTargetRange( m_object );
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    // FIXME: mega ugly, because mapper not given in cnostructor!
    m_painter.setCoordinateMapper( getCoordinateMapper() );

    /* recreate index */
    m_hoverIndex = new HoverIndex();
    m_painter.setHoverIndex( m_hoverIndex );

    m_painter.paint( gc, m_object );
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    if( m_hoverIndex == null )
      return null;

    return m_hoverIndex.findElement( pos );
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