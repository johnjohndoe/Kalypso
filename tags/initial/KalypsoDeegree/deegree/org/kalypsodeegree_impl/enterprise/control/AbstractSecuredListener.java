/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

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

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.enterprise.control;

import org.deegree.enterprise.control.FormEvent;
import org.deegree_impl.tools.Debug;

/**
 * This abstract <tt>Listener</tt> ensures that only privileged
 * users have access to the services provided by derived
 * implementations.
 * <p>
 * - This class is extended by Listeners that may require security checks.
 * - The extended classes may then be extended again by an concrete 
 *   implementation (with a certain security model) that overwrites the
 *   "actionPerformed"-method to perform the security check.  
 * <p>
 * @author <a href="mschneider@lat-lon.de">Markus Schneider</a>
 */
public abstract class AbstractSecuredListener extends AbstractListener {

	/**
	 * Has to be overwritten by the concrete <tt>Listener</tt>.
	 * <p>
	 * @param event
	 */
	public void actionPerformed (FormEvent event) {
		Debug.debugMethodBegin ();

		// security checks go here 
		performPrivilegedOperation (event); 

		Debug.debugMethodEnd ();
	}
	
	public abstract void performPrivilegedOperation (FormEvent event);
}