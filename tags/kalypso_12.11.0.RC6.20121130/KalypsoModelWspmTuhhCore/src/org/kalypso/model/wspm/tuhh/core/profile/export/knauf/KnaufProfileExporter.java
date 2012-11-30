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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf;

import java.io.File;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.builders.KnaufCalculationBeanBuilder;

/**
 * @author Dirk Kuch
 */
public class KnaufProfileExporter implements ICoreRunnableWithProgress
{
  private final KnaufCalculation m_calculation;

  private final File m_destination;

  public KnaufProfileExporter( final KnaufCalculation calculation, final File destination )
  {
    m_calculation = calculation;
    m_destination = destination;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    final KnaufCalculationBeanBuilder builder = new KnaufCalculationBeanBuilder( m_calculation );
    stati.add( builder.execute( new SubProgressMonitor( monitor, 1 ) ) );

    final KnaufBeanSerializer serializer = new KnaufBeanSerializer( builder.getBeans(), m_destination );
    stati.add( serializer.execute( monitor ) );

    return StatusUtilities.createStatus( stati, Messages.getString("KnaufProfileExporter_0") ); //$NON-NLS-1$
  }
}