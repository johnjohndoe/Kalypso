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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;

import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;

/**
 * removes obsolete elements from hydrotope.gml
 * 
 * <pre>
 * invalid members:
 *  - drainageType
 *  - sudLinkMember
 *  - hydType
 * 
 * invalid name spaces:
 *   - http://sourceforge.kalypso.org/schemata/hydrology/suds
 * 
 * </pre>
 * 
 * @author Dirk Kuch
 */
public class RemoveObsoleteHydrotopesMembers extends AbstractRemoveOsoleteXmlMembersWorker implements ICoreRunnableWithProgress
{

  public RemoveObsoleteHydrotopesMembers( final File gmlFile )
  {
    super( gmlFile );
  }

  @Override
  protected String[] getInvalidNamespaces( )
  {
    return new String[] { "http://sourceforge.kalypso.org/schemata/hydrology/suds" }; //$NON-NLS-1$
  }

  @Override
  protected String getRootElementLocalName( )
  {
    return "NAHydrotop"; //$NON-NLS-1$
  }

  @Override
  protected String[] getInvalidProperties( )
  {
    return new String[] { "drainageType", "sudLinkMember", "hydType" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

}
