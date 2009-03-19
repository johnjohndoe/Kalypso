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
package org.kalypso.gaja3d.service.internal;

import java.net.URL;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.osgi.framework.Bundle;

public class Gaja3dServiceUrlCatalog extends AbstractUrlCatalog {
	public static final String GAJA3D_NS = "http://org.kalypso.gaja3d.service";
	final static public String GAJA3D_NS_PREFIX = "gaja3d";

	@Override
	protected void fillCatalog(final Class<?> myClass,
			final Map<String, URL> catalog, final Map<String, String> prefixes) {
		final Bundle bundle = Platform.getBundle("org.kalypso.gaja3d.service");
		final URL entry = FileLocator.find(bundle, new Path("schema/gaja3d/wpsExecute_gaja3d_0.4.xsd"), null);
		catalog.put(GAJA3D_NS, entry);
		prefixes.put(GAJA3D_NS, GAJA3D_NS_PREFIX);
	}

}
