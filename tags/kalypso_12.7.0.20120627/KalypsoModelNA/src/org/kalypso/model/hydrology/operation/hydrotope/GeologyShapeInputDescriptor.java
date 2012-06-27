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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.io.File;
import java.nio.charset.Charset;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyImportOperation.InputDescriptor;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

public class GeologyShapeInputDescriptor extends AbstractShapeInputDescriptor<GM_MultiSurface> implements InputDescriptor
{
  private final String m_maxPerculationRateColumn;

  private final String m_gwFactorColumn;

  public GeologyShapeInputDescriptor( final File shapeFile, final String maxPerculationRateColumn, final String gwFactorColumn, final String crs, final Charset charset )
  {
    super( shapeFile, crs, charset );

    m_maxPerculationRateColumn = maxPerculationRateColumn;
    m_gwFactorColumn = gwFactorColumn;
  }

  @Override
  public double getGWFactor( final int index ) throws CoreException
  {
    final Object property = getProperty( index, m_gwFactorColumn );
    return ShapeImportDescriptiorHelper.parseAsDouble( property );
  }

  @Override
  public double getMaxPerkulationsRate( final int index ) throws CoreException
  {
    final Object property = getProperty( index, m_maxPerculationRateColumn );
    return ShapeImportDescriptiorHelper.parseAsDouble( property );
  }
}