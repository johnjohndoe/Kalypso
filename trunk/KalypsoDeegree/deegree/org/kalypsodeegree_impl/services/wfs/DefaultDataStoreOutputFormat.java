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
package org.deegree_impl.services.wfs;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamSource;

import org.deegree.gml.GMLFeatureCollection;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.services.wfs.DataStoreOutputFormat;
import org.deegree.services.wfs.WFSConstants;
import org.deegree.tools.Parameter;
import org.deegree.tools.ParameterList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFeatureCollection_Impl;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLFeatureAdapter;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class DefaultDataStoreOutputFormat implements DataStoreOutputFormat
{
  /**
   * formats the data store at the values of the HashMap into one single data
   * structure.
   */
  public Object format( HashMap map, ParameterList paramList ) throws Exception
  {
    Debug.debugMethodBegin();
    Iterator iterator = map.values().iterator();

    FeatureCollection fc = FeatureFactory.createFeatureCollection( "id", 10000 );

    // for each table within the received HashMap
    Parameter p = null;

    while( iterator.hasNext() )
    {
      ParameterList pl = (ParameterList)iterator.next();
      p = pl.getParameter( WFSConstants.FEATURECOLLECTION );
      FeatureCollection tfc = (FeatureCollection)p.getValue();
      fc.appendFeatures( tfc );
    }

    p = paramList.getParameter( WFSConstants.NAMESPACE );
    String sr = "";

    Map namesp = new HashMap();
    Map pref = new HashMap();
    if( p != null )
    {
      sr = (String)p.getValue();
      StringTokenizer st = new StringTokenizer( sr, ":=" );
      st.nextToken();
      String pre = st.nextToken();
      String ns = st.nextToken();
      namesp.put( fc.getFeature( 0 ).getFeatureType().getName(), ns );
      pref.put( fc.getFeature( 0 ).getFeatureType().getName(), pre );
    }

    ByteArrayOutputStream bos = new ByteArrayOutputStream( fc.getSize() * 1000 );
    GMLFeatureAdapter.export( fc, namesp, pref, new HashMap(), bos );
    String s = new String( bos.toByteArray(), "UTF-8" );

    Document doc = null;
    p = paramList.getParameter( WFSConstants.FILTER );
    if( p != null )
    {
      doc = xsltTransformGetFeature( s, (String)p.getValue() );
    }
    else
    {
      doc = XMLTools.parse( new StringReader( s ) );
    }

    // create the feature collection that will contain all requested
    // features (table rows)
    GMLFeatureCollection gmlfc = new GMLFeatureCollection_Impl( doc.getDocumentElement() );

    Debug.debugMethodEnd();

    return gmlfc;
  }

  /**
   * transforms the response to a GetRecord/Feature request using a predefined
   * xslt-stylesheet
   */
  protected Document xsltTransformGetFeature( String gml, String xsltURL )
  {
    Debug.debugMethodBegin();

    Document document = null;

    try
    {
      //document = XMLTools.parse( new StringReader( gml ) );

      // Use the static TransformerFactory.newInstance() method to instantiate
      // a TransformerFactory. The javax.xml.transform.TransformerFactory
      // system property setting determines the actual class to instantiate --
      // org.apache.xalan.transformer.TransformerImpl.
      TransformerFactory tFactory = TransformerFactory.newInstance();

      // Use the TransformerFactory to instantiate a Transformer that will work
      // with
      // the stylesheet you specify. This method call also processes the
      // stylesheet
      // into a compiled Templates object.
      URL url = new URL( xsltURL );
      Transformer transformer = tFactory.newTransformer( new StreamSource( url.openStream() ) );

      // Use the Transformer to apply the associated Templates object to an XML
      // document
      // (foo.xml) and write the output to a file (foo.out).
      StringWriter sw = new StringWriter();
      DOMResult dr = new DOMResult( document );
      transformer.transform( new StreamSource( new StringReader( gml ) ), dr );
      document = (Document)dr.getNode();

      //document = XMLTools.parse( new StringReader( sw.toString() ) );
    }
    catch( Exception e )
    {
      Debug.debugException( e, "an error/fault body for the soap message will be created" );
    }

    Debug.debugMethodEnd();
    return document;
  }
}