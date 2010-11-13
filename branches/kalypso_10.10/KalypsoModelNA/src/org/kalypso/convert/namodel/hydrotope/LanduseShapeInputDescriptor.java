/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.convert.namodel.hydrotope;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.binding.suds.AbstractSud;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

public class LanduseShapeInputDescriptor implements InputDescriptor
{
  private final String m_landuseclassColumn;

  private final String m_drainageTypeColumn;

  private final String m_corrSealingColumn;

  private final ShapeWithHash m_shapeWithHash;

  public LanduseShapeInputDescriptor( final File shapeFile, final String landuseclassColumn, final String corrSealingColumn, final String drainageTypeColumn )
  {
    // TODO: important: let user enter crs of shape and transform read geometry to kalypso crs.
    final String crs = null;

    m_shapeWithHash = new ShapeWithHash( shapeFile, crs );
    m_landuseclassColumn = landuseclassColumn;
    m_corrSealingColumn = corrSealingColumn;
    m_drainageTypeColumn = drainageTypeColumn;
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getName(int)
   */
  @Override
  public String getName( final int index )
  {
    return "" + index; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getDescription(int)
   */
  @Override
  public String getDescription( final int index )
  {
    return m_shapeWithHash.getDescription();
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getGeometry(int)
   */
  @Override
  public GM_MultiSurface getGeometry( final int index ) throws CoreException
  {
    return m_shapeWithHash.getGeometry( index );
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getSealingCorrectionFactor(int)
   */
  @Override
  public double getSealingCorrectionFactor( final int index ) throws CoreException
  {
    final Object property = getProperty( index, m_corrSealingColumn );
    final Double value = ShapeImportDescriptiorHelper.parseAsDouble( property );
    if( value != null )
      return value;
    final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.LanduseShapeInputDescriptor.4", m_corrSealingColumn ); //$NON-NLS-1$
    throw new CoreException( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getDrainageType(long)
   */
  @Override
  public String getDrainageType( final int index ) throws CoreException
  {
    final Object property = getProperty( index, m_drainageTypeColumn );
    return property == null ? null : property.toString();
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getLanduseclass(long)
   */
  @Override
  public String getLanduseclass( final int index ) throws CoreException
  {
    final Object property = getProperty( index, m_landuseclassColumn );
    return property == null ? null : property.toString();
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getSize()
   */
  @Override
  public int size( ) throws CoreException
  {
    return m_shapeWithHash.size();
  }

  private Object getProperty( final int index, final String property ) throws CoreException
  {
    return m_shapeWithHash.getProperty( index, property );
  }

  /**
   * @see org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor#getSuds(int)
   */
  @Override
  public AbstractSud[] getSuds( final int index )
  {
    // nothing to do
    return new AbstractSud[] {};
  }

}