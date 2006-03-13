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
package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @deprecated use ant task instead in your model-configuration
 * @author belger
 */
public abstract class AbstractTransformation implements ITransformation
{
  private Properties m_properties;

  /**
   * @see org.kalypso.util.transformation.ITransformation#setProperties(java.util.Properties)
   */
  public final void setProperties( final Properties props )
  {
    m_properties = props;
  }

  /**
   * @see org.kalypso.util.transformation.ITransformation#transform(java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transform( final BufferedWriter msgWriter, final BufferedWriter logWriter, final IProgressMonitor monitor )
      throws TransformationException
  {
    transformIntern( m_properties, msgWriter, logWriter, monitor );
  }

  /**
   * @see ITransformation#transform(BufferedWriter, BufferedWriter, IProgressMonitor)
   * 
   * @param properties
   * @param msgWriter
   * @param logWriter
   * @param monitor
   * @throws TransformationException
   */
  protected abstract void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException;
}