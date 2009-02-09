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
package org.kalypso.simulation.grid;

import java.io.File;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.digester.RegexMatcher;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.WildcardFilter;
import org.apache.tools.ant.types.selectors.FilenameSelector;
import org.apache.tools.ant.util.RegexpPatternMapper;
import org.apache.tools.ant.util.regexp.RegexpMatcherFactory;
import org.apache.tools.ant.util.regexp.RegexpUtil;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.process.IProcess;
import org.kalypso.contribs.java.util.regex.RegexpUtilities;

public class TestSimpleGridProcess extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testGridProcess() throws Exception {
		final String processFactoryId = "org.kalypso.simulation.gridprocess";
		final File tmpDir = new File("test");
		System.setProperty( "GLOBUS_LOCATION", "d:/workspace3.4/org.globus.ws.core" );
		// final URL exeURL = new
		// File("d:\\eclipse3.4\\bin\\RMA10Sk_35").toURI().toURL();
		final URL exeURL = FileLocator.find(Activator.getDefault().getBundle(),
				new Path("RMA10Sk_35"), null);
		final IProcess process = KalypsoCommonsExtensions.createProcess(
				processFactoryId, tmpDir, exeURL, null);
		process.environment().put("OMP_NUM_THREADS", "4");
		((SimpleGridProcess)process).addInput( FileLocator.find(Activator.getDefault().getBundle(),
                new Path("model.2d"), null));
		((SimpleGridProcess)process).addInput( FileLocator.find(Activator.getDefault().getBundle(),
            new Path("control.r10"), null));
		((SimpleGridProcess)process).addOutput( "A*.2d" );
		process.startProcess(System.out, System.err, null, null);
	}

}
