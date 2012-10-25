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
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.PartTypeAccessor;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ProfileObjectPainter;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author Gernot Belger
 */
public abstract class WspObjectsLayer extends AbstractProfilLayer
{
  private final ProfileObjectPainter m_painter;

  private final IWspLayerData m_wspData;

  private final String m_type;

  public WspObjectsLayer( final String id, final IProfile profile, final IWspLayerData wspData, final String type )
  {
    super( id, profile );

    m_wspData = wspData;
    m_type = type;

    m_painter = new ProfileObjectPainter( profile, type );

    final PartTypeAccessor info = m_painter.getInfo();
    final String title = info.getTypeLabel();
    setTitle( title );
  }

  @Override
  public IDataRange<Double> getDomainRange( )
  {
    final IProfileObject[] allObjects = findAllObjects();

    return m_painter.getDomainRange( allObjects );
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange< ? > domainIntervall )
  {
    final IProfileObject[] allObjects = findAllObjects();

    return m_painter.getTargetRange( allObjects );
  }

  private IProfileObject[] findAllObjects( )
  {
    final Collection<IProfileObject> allObjects = new ArrayList<>();

    final Object[] activeElements = m_wspData.getActiveElements();
    for( final Object activeElement : activeElements )
    {
      final TuhhResultDataElement element = (TuhhResultDataElement)activeElement;
      final IProfileObject[] objects = element.getProfileObjects( m_type );

      allObjects.addAll( Arrays.asList( objects ) );
    }

    return allObjects.toArray( new IProfileObject[allObjects.size()] );
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final Object[] activeElements = m_wspData.getActiveElements();
    for( final Object activeElement : activeElements )
    {
      final TuhhResultDataElement element = (TuhhResultDataElement)activeElement;
      final IProfileObject[] objects = element.getProfileObjects( m_type );
      for( final IProfileObject object : objects )
      {
        m_painter.paint( gc, object );

        // FIXME: create edit infos!
      }
    }
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