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
package org.deegree_impl.clients.wcasclient.control;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.URL;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.enterprise.control.AbstractSecuredListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * This <tt>Listener</tt> reacts on 'deleteService'-events, queries the WCAS
 * and passes the service data on to be displayed by the JSP.   
 * <p>
 * @author <a href="mschneider@lat-lon.de">Markus Schneider</a>
 */
public class DeleteServiceListener extends AbstractSecuredListener {
	
	public void performPrivilegedOperation (FormEvent event) {
		Debug.debugMethodBegin();

		try {
			String serviceId = null;
			URL catalogURL = null;

			// decode RPC-event
			if (event instanceof RPCWebEvent) {
				RPCWebEvent ev = (RPCWebEvent) event;
				RPCMethodCall rpcCall = ev.getRPCMethodCall();
				RPCParameter [] params = rpcCall.getParameters ();

				if (params.length != 2) {
					throw new RPCException ("Invalid RPC. Exactly two " +
						"'param'-elements below 'params' is required.");
				}
				for (int i = 0; i < 2; i++) {
					if (!(params [i].getValue () instanceof String)) {
						throw new RPCException ("Invalid RPC. 'param'-elements " +
							"below 'params' must contain string-values.");
					}
				}
				catalogURL = CSWClientConfiguration.getInstance().
					getCatalogServerAddress ((String) params [0].getValue ());
				serviceId = (String) params [1].getValue ();
			} else {
				throw new Exception ("No valid RPC event has been received.");
			}

			deleteService (serviceId, catalogURL);
			getRequest().setAttribute (
				"MESSAGE", "Der Service mit der ID '" + serviceId + "' wurde entfernt.");
		} catch (Exception e) {
			getRequest().setAttribute ("SOURCE", this.getClass ().getName ());
			getRequest().setAttribute (
				"MESSAGE", "Die Serviceadministration konnte nicht " +
				"initialisiert werden, da ein Fehler augetreten ist.<br><br>" +
				"Die Fehlermeldung lautet: <code>" + e.getMessage () + "</code>");
			setNextPage ("admin_error.jsp");
		}

		Debug.debugMethodEnd ();
	}

	/**
	 * Deletes the specified service from the WCAS.
	 * <p>
	 * @param serviceId
	 * @param catalogURL
	 * @throws IOException
	 * @throws SAXException
	 * @throws XMLParsingException
	 */
	protected void deleteService (String serviceId, URL catalogURL) throws IOException, 
		SAXException, XMLParsingException {
		Debug.debugMethodBegin();

		// build WCAS-request
		String deleteRequest = 
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
			"<Transaction xmlns:ogc=\"http://www.opengis.net/ogc\">" +
			"<Delete type=\"ISO19119\">" +
			"<ogc:Filter>" +
			"<ogc:PropertyIsEqualTo>" +
			"<ogc:PropertyName>ISO19119/fileIdentifier</ogc:PropertyName>" +
			"<ogc:Literal><![CDATA[" + serviceId + "]]></ogc:Literal>" +
			"</ogc:PropertyIsEqualTo>" +
			"</ogc:Filter>" +
			"</Delete>" +
			"</Transaction>";

		// open connection and send request
		NetWorker netWorker = new NetWorker (catalogURL, deleteRequest);
     
		// server response -> DOM 
		InputStreamReader reader = new InputStreamReader (netWorker.getInputStream(), "UTF-8");
		Document doc = XMLTools.parse (reader);
		reader.close();

		// print out result
		DOMPrinter.printNode (new PrintWriter (System.out), doc);			

		Debug.debugMethodEnd ();			
	}
}
