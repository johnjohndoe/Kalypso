/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.contribs.java.lang.ISupplier;

/**
 * Exporters can create one or more {@link org.kalypso.metadoc.IExportableObject} and export them to some {@link org.kalypso.metadoc.IExportTarget}. This is actually used as a
 * wrapper over {@link org.kalypso.metadoc.IExportableObjectFactory} which is exhibited as an extension point (org.kalypso.metadoc.exporter)
 * 
 * @author schlienger
 */
public interface IExporter extends IExportableObjectFactory, IExecutableExtension
{
  /** used as label */
  public String getName();

  /** used as tooltip */
  public String getDescription();
  
  /** used as icon */
  public ImageDescriptor getImageDescriptor();

  /**
   * Init this exporter with context dependent data. The supplier object is used
   * to get the data on a per key basis.
   * <p>
   * For instance, a GIS-Exporter will probably want to have a list of features. It would
   * get it by doing the following:
   * <code>
   * 	features = supplier.supply( "featureList" );
   * </code>
   */
  public void init( final ISupplier supplier ) throws CoreException;
}
