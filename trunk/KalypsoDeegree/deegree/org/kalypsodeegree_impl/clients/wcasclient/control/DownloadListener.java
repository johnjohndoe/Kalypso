/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software;you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation;either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY;without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library;if not, write to the Free Software
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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLGeometry;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.xml.ElementList;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.MissingISO19119EntryException;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.configuration.Download;
import org.deegree_impl.clients.wcasclient.configuration.TextComponent;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.gml.GMLBox_Impl;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.deegree_impl.services.wfs.filterencoding.SpatialOperation;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.deegree_impl.tools.ZipUtil;
import org.deegree_impl.tools.mail.EMailMessage;
import org.deegree_impl.tools.mail.MailHelper;
import org.deegree_impl.tools.mail.MailMessage;
import org.deegree_impl.tools.mail.SendMailException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This Listener is called if a user like to download one or more datasets
 * assigned to the metadata he had find through a catalog
 * 
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class DownloadListener extends AbstractListener
{

  /**
   * This method is called either to search for metadata or to lookup gemet
   * keywords.
   */
  public void actionPerformed( FormEvent event )
  {
    //        Debug.level = Debug.ALL;
    Debug.debugMethodBegin();

    RPCWebEvent rpcEvent = (RPCWebEvent)event;
    String msg = null;
    try
    {
      if( !validateRequest( rpcEvent ) )
      {
        this.setNextPage( this.getAlternativeNextPage() );
        Debug.debugMethodEnd();
        return;
      }
      // check if the user owns the required rights
      msg = proofAuthorization( rpcEvent );
    }
    catch( Exception e )
    {
      gotoErrorPage( e.toString() );
      Debug.debugMethodEnd();
      return;
    }

    if( !msg.equals( "" ) )
    {
      // user doesn't have the authorization to download the ordered
      // datasets
      this.setNextPage( this.getAlternativeNextPage() );
      this.getRequest().setAttribute( Constants.MESSAGE, msg );
    }
    else
    {
      String catalog = null;
      try
      {
        HashMap gfrl = new HashMap();
        HashMap fids = getCatalogIdAssociations( rpcEvent );
        Iterator iterator = fids.keySet().iterator();
        // create a GetFeature request for each ordered dataset
        while( iterator.hasNext() )
        {
          catalog = (String)iterator.next();
          ArrayList list = (ArrayList)fids.get( catalog );
          FeatureTemplate[] ft = (FeatureTemplate[])list.toArray( new FeatureTemplate[list.size()] );
          HashMap tmp = getWFSGetFeatureCalls( catalog, ft );
          if( tmp.get( "null" ) != null )
          {
            // if the HashMap contains an entry assigned to "null"
            // there is at least one dataset that can't be accessed
            // through WFS
            TextComponent tc = CSWClientConfiguration.getInstance().getTextComponent();
            StringBuffer s = new StringBuffer( 500 );
            try
            {
              s.append( tc.getMessage( "text/download/notAvailable" ) );
              s.append( "<BR><BR>" );
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
            s.append( "<b>" ).append( tmp.get( "null" ) ).append( "</b>" );
            throw new MissingISO19119EntryException( s.toString() );
          }
          gfrl.putAll( tmp );
        }
        // perform dataloading in a own thread
        LoadController cntr = new LoadController( gfrl );
        cntr.start();
        // inform the user that the download has been started
        CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
        TextComponent tc = conf.getTextComponent();
        String s = tc.getMessage( "text/download/orderConfirmation" );
        getRequest().setAttribute( Constants.MESSAGE, s );
      }
      catch( MissingISO19119EntryException me )
      {
        Debug.debugException( me, "" );
        this.getRequest().setAttribute( Constants.MESSAGE, me.getMessage() );
        this.setNextPage( "message_intern.jsp" );
      }
      catch( Exception e )
      {
        Debug.debugException( e, "" );
        this.getRequest().setAttribute( Constants.MESSAGE, e.toString() );
        gotoErrorPage( e.toString() );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * validates the request to be performed
   */
  private boolean validateRequest( RPCWebEvent event )
  {
    return event != null;
  }

  /**
   * checks if the current user is authorized to to download the ordered
   * datasets
   */
  private String proofAuthorization( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = event.getRPCMethodCall().getParameters();

    StringBuffer sb = sb = new StringBuffer();
    for( int i = 0; i < params.length; i++ )
    {
      RPCStruct struct = (RPCStruct)params[i].getValue();
      String fileIdentifier = (String)struct.getMember( Constants.RPC_ID ).getValue();
      String msg = isAuthorizied( fileIdentifier );
      if( msg != null )
      {
        // collect NoAuthorization messages
        sb.append( "<BR/>" + msg );
      }
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

  /**
   * validates if the current user iss allowed to display the selected datasets
   * with a WMS
   * 
   * @return
   */
  protected String isAuthorizied( String id ) throws CatalogClientException
  {
    // returning <tt>null</tt> indicates that an authorization exists
    return null;
  }

  /**
   * performs the access to the data marked at the shopping card
   */
  private HashMap getCatalogIdAssociations( RPCWebEvent event ) throws CatalogClientException,
      Exception
  {
    Debug.debugMethodBegin();

    RPCParameter[] params = event.getRPCMethodCall().getParameters();

    HashMap fids = new HashMap();
    for( int i = 0; i < params.length; i++ )
    {

      RPCStruct struct = (RPCStruct)params[i].getValue();
      String catalog = (String)struct.getMember( Constants.RPC_CATALOG ).getValue();
      String fileIdentifier = (String)struct.getMember( Constants.RPC_ID ).getValue();
      String title = (String)struct.getMember( Constants.RPC_TITLE ).getValue();
      RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();
      GM_Envelope bbox = extractBoundinbox( bboxStruct );

      FeatureTemplate ft = new FeatureTemplate( fileIdentifier, title, bbox );
      // get feature templates associated with the current catalog and
      // add the current feature template containing the features ID
      // and bbox to the list. If necessary, create a new list
      ArrayList list = (ArrayList)fids.get( catalog );
      if( list == null )
      {
        list = new ArrayList();
      }
      list.add( ft );
      fids.put( catalog, list );

    }

    Debug.debugMethodEnd();
    return fids;
  }

  /**
   * returns the call to be used to perform a GetFeature request for the passed
   * feature template.
   * 
   * @param catalog
   *          catalog where the required ISO 19119 data are stored
   * @param ft
   *          feature template to find the serving WFS
   */
  private HashMap getWFSGetFeatureCalls( String catalog, FeatureTemplate[] ft )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String href = null;
    String version = null;
    HashMap reqList = new HashMap();
    try
    {
      // iterate over each feature type (ID) that was ordered
      for( int z = 0; z < ft.length; z++ )
      {
        String[] ids = new String[]
        { ft[z].getId() };
        String casReq = ISO19119RequestFactory.createRequest( "Full", "WFS", ids, "Query" );
        URL url = CSWClientConfiguration.getInstance().getCatalogServerAddress( catalog );

        NetWorker nw = new NetWorker( url, casReq );
        Reader reader = new InputStreamReader( nw.getInputStream() );
        Document doc = XMLTools.parse( reader );
        NodeList nl = doc.getElementsByTagName( "serviceTypeVersion" );
        // get service version; if not available use 1.1.0 as default
        if( nl.getLength() > 0 )
        {
          version = XMLTools.getStringValue( nl.item( 0 ) );
        }
        else
        {
          version = "1.0.0";
        }
        nl = doc.getElementsByTagName( "operationMetadata" );
        // loop over all supported operation
        for( int i = 0; i < nl.getLength(); i++ )
        {
          String s1 = XMLTools.getRequiredStringValue( "operationName", null, nl.item( i ) );
          String s2 = XMLTools.getRequiredStringValue( "nameNameSpace", null, nl.item( i ) );
          // is required operation?
          if( s1.equals( "Query" ) && s2.equals( "OGC" ) )
          {
            ElementList ell = XMLTools.getChildElementsByName( "DCP", null, nl.item( i ) );
            // get web address/accesspoint for GetMap operation
            for( int k = 0; k < ell.getLength(); k++ )
            {
              String s3 = XMLTools.getAttrValue( "type", ell.item( k ) );
              if( s3.equalsIgnoreCase( "POST" ) || s3.equalsIgnoreCase( "HTTPPOST" ) )
              {
                Node node = ell.item( k ).getElementsByTagName( "linkage" ).item( k );
                href = XMLTools.getAttrValue( "href", node );
                // leaf the sub loop
                break;
              }
            }
          }
          if( href != null )
            break;
        }

        if( href != null )
        {
          WFSGetFeatureRequest gfr = createGetFeatureRequest( ft[z], version );
          reqList.put( href, gfr );
        }
        else
        {
          reqList.put( "null", ft[z].getTitle() );
        }
      }

    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
      throw new CatalogClientException( "", e );
    }

    Debug.debugMethodEnd();
    return reqList;
  }

  /**
   * creates a GetFeature request considering the feature type (ID) and the
   * bounding box encapsulated in the passed <tt>FeatureTemplate</tt>
   * 
   * @param ft
   *          FeatureTemplate
   */
  private WFSGetFeatureRequest createGetFeatureRequest( FeatureTemplate ft, String version )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    WFSGetFeatureRequest gfr = null;

    try
    {
      // create BBOx as spatial filter condition
      StringReader reader = new StringReader( GMLAdapter.export( ft.getEnvelope() ) );
      GMLGeometry gml = new GMLBox_Impl( XMLTools.parse( reader ).getDocumentElement() );
      Operation op = new SpatialOperation( OperationDefines.BBOX, new PropertyName( "GEOM" ), gml );
      Filter filter = new ComplexFilter( op );
      // create query using the ID of the FeatureTemplate as typename
      WFSQuery query = WFSProtocolFactory.createQuery( null, null, version, ft.getId(), filter );
      IDGenerator idg = IDGenerator.getInstance();
      // create WFS GetFeature request
      gfr = WFSProtocolFactory.createWFSGetFeatureRequest( "1.0.0", "" + idg.generateUniqueID(),
          null, null, "GML2", null, null, -1, 0, new WFSQuery[]
          { query } );
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
      throw new CatalogClientException( "couldn't create GetFeature request " + "for: "
          + ft.getId() + "\n" + ft.getEnvelope(), e );
    }

    Debug.debugMethodEnd();
    return gfr;
  }

  /**
   * extracts a bounding box from the passed RPCStruct
   */
  private GM_Envelope extractBoundinbox( RPCStruct bboxStruct )
  {
    Debug.debugMethodBegin();

    Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
    Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
    Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
    Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();
    GM_Envelope bbox = GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(),
        maxx.doubleValue(), maxy.doubleValue() );
    Debug.debugMethodEnd();
    return bbox;
  }

  ///////////////////////////////////////////////////////////////////////////
  //                         inner class //
  ///////////////////////////////////////////////////////////////////////////

  /**
   * class that handles the loading of the requested data from the responsible
   * WFS. The class also informs the user who had requested the data via email
   * that the data are ready for download if everything had worked fine.
   * otherwise the the user and the administrator will be informed about the
   * problem that had occured.
   */
  private class LoadController extends Thread
  {

    private HashMap gfr = null;

    /**
     * initialize the LoadController with an association of web addresses with
     * GetFeature requests.
     */
    LoadController( HashMap gfr )
    {
      this.gfr = gfr;
    }

    public void run()
    {
      Debug.debugMethodBegin();
      ArrayList zipFiles = new ArrayList();

      Iterator iterator = gfr.keySet().iterator();

      while( iterator.hasNext() )
      {
        String addr = (String)iterator.next();
        WFSGetFeatureRequest getFeat = (WFSGetFeatureRequest)gfr.get( addr );
        FeatureCollection fc = null;
        try
        {
          // get data from a WFS
          URL url = new URL( addr );
          String s = ( (Marshallable)getFeat ).exportAsXML();
          NetWorker nw = new NetWorker( url, s );
          Reader reader = new InputStreamReader( nw.getInputStream() );
          Document doc = validateResult( reader );
          // transform data and store as shape file
          GMLDocument gmlDoc = new GMLDocument_Impl( doc );
          fc = FeatureFactory.createFeatureCollection( gmlDoc.getRoot() );
        }
        catch( Exception e )
        {
          // remove all created zip files
          rollback( zipFiles );
          Debug.debugException( e, "create shape file: " + getFeat.getQuery()[0].getTypeName() );
          try
          {
            sendErrorMail( e, addr, ( (Marshallable)getFeat ).exportAsXML() );
          }
          catch( Exception e2 )
          {
            Debug.debugException( e2, "" );
          }
          break;
        }
        try
        {
          String filename = storeFC( fc, getFeat.getQuery()[0].getTypeName() );
          // add name of the created zipfile to an ArrayList to be used
          // to inform the user where to download the data
          zipFiles.add( filename );
        }
        catch( Exception e )
        {
          // remove all created zip files
          rollback( zipFiles );
          Debug.debugException( e, "create zip file" );
          try
          {
            sendErrorMail( e, null, getFeat.getQuery()[0].getTypeName() );
          }
          catch( Exception e2 )
          {
            Debug.debugException( e2, "" );
          }
          break;
        }
      }

      try
      {
        sendSuccessMail( zipFiles );
      }
      catch( Exception e )
      {
        Debug.debugException( e, "sendSuccessMail" );
      }

      Debug.debugMethodEnd();
    }

    /**
     * stores the passed feature collection as zipped shape and returns the file
     * name
     * 
     * @param fc
     *          feature collection to be stored
     * @param typeName
     *          name of the feature type requested from the WFS
     */
    private String storeFC( FeatureCollection fc, String typeName ) throws CatalogClientException
    {
      Debug.debugMethodBegin();

      String zipName = null;
      String[] fileNames = null;
      String dir = CSWClientConfiguration.getInstance().getDownload().getStorageDirectory();
      try
      {
        // create and write shape
        long l = IDGenerator.getInstance().generateUniqueID();
        String shapeBaseName = typeName + System.currentTimeMillis() + "" + l;
        ShapeFile shp = new ShapeFile( dir + "/" + shapeBaseName, "rw" );
        shp.writeShape( fc );
        shp.close();

        // create zip file from .shp, .shx and .dbf
        zipName = shapeBaseName + ".zip";

        fileNames = new String[]
        { shapeBaseName + ".shp", shapeBaseName + ".shx", shapeBaseName + ".dbf" };

        // create zip archive
        ZipUtil zu = new ZipUtil();
        zu.doZip( dir, zipName, fileNames );
      }
      catch( Exception e )
      {
        // remove the created shape files
        for( int i = 0; i < 3; i++ )
        {
          File file = new File( dir + "/" + fileNames[i] );
          file.delete();
        }
        // remove the zip file
        File file = new File( dir + "/" + zipName );
        file.delete();
        throw new CatalogClientException( "", e );
      }

      Debug.debugMethodEnd();
      return zipName;
    }

    /**
     * validates the result of the catalog request and returns a
     * <tt>Document</tt> or a message string depending on the results content.
     */
    private Document validateResult( Reader reader ) throws IOException, SAXException, Exception
    {
      Debug.debugMethodBegin();

      BufferedReader br = new BufferedReader( reader );
      StringBuffer sb = new StringBuffer( 50000 );
      String s = null;
      while( ( s = br.readLine() ) != null )
      {
        sb.append( s );
      }
      s = sb.toString();

      Document doc = null;

      if( s.indexOf( "<Exception>" ) >= 0 )
      {
        throw new Exception( "couldn't get data from WFS:\n" + s );
      }
      else
      {
        StringReader sr = new StringReader( s );
        doc = XMLTools.parse( sr );
        sr.close();
      }

      Debug.debugMethodEnd();

      return doc;
    }

    /**
     * deletes all files associated to the download order
     * 
     * @param zipFiles
     *          list of zip files associated to the download order
     */
    private void rollback( ArrayList zipFiles )
    {
      Debug.debugMethodBegin();

      try
      {
        String dir = CSWClientConfiguration.getInstance().getDownload().getStorageDirectory();
        for( int i = 0; i < zipFiles.size(); i++ )
        {
          String fn = (String)zipFiles.get( i );
          File file = new File( dir + "/" + fn );
          file.delete();
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, "rollback" );
      }

      Debug.debugMethodEnd();
    }

    /**
     * sends a mail to inform the administrator and the user about an error that
     * raised performing the data access
     */
    private void sendErrorMail( Exception e, String addr, String target ) throws SendMailException
    {
      Debug.debugMethodBegin();

      String st = StringExtend.stackTraceToString( e.getStackTrace() );
      String message = null;
      if( addr == null )
      {
        message = "Shapefile couldn't be created: " + target + "\n" + st;
      }
      else
      {
        message = "GetFeature request couldn't be performed\n" + addr + "\n" + target + "\n" + st;
      }

      CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
      Download download = conf.getDownload();
      String mailAddress = null;

      // send message to the user
      MailMessage mm = new EMailMessage( download.getMailFrom(), mailAddress, download
          .getMailSubject(), message );
      MailHelper.createAndSendMail( mm, download.getMailHost() );

      // send message to administrator
      mm = new EMailMessage( download.getMailFrom(), download.getMailFrom(), download
          .getMailSubject(), message );
      MailHelper.createAndSendMail( mm, download.getMailHost() );

      Debug.debugMethodEnd();
    }

    /**
     * sends a mail to the user that the data download succeded and informs him
     * where to download the created files
     * 
     * @param files
     *          list of created files (names)
     * 
     * @throws SendMailException
     */
    private void sendSuccessMail( ArrayList files ) throws SendMailException
    {
      Debug.debugMethodBegin();

      CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
      Download download = conf.getDownload();

      // create mesage for informing the user
      URL address = download.getOnlineResource();
      StringBuffer sb = new StringBuffer( "Sie können die von Ihnen angeforderten " );
      sb.append( "Datensätze über folgende links herunter laden\n:" );

      for( int i = 0; i < files.size(); i++ )
      {
        String file = (String)files.get( i );
        sb.append( NetWorker.url2String( address ) + "?file=" + file + "\n" );
      }

      sb.append( "\nDie Daten werden " + ( download.getLifeTime() / 60 ) );
      sb.append( " Stunden für Sie bereit gehalten und anschliessend gelöscht." );

      // get the users mailaddress
      //            Administrator admin = conf.getSecurityAdmin();
      //            User u = admin.getUser( user );
      //            String mailAddress = u.getEmail();
      String mailAddress = null;

      // send message to the user
      MailMessage mm = new EMailMessage( download.getMailFrom(), mailAddress, download
          .getMailSubject(), sb.toString() );
      MailHelper.createAndSendMail( mm, download.getMailHost() );

      Debug.debugMethodEnd();
    }

  }

  /**
   * little helper class to store association between IDs and bounding boxes
   */
  private class FeatureTemplate
  {

    private String id = null;

    private String title = null;

    private GM_Envelope bbox = null;

    FeatureTemplate( String id, String title, GM_Envelope bbox )
    {
      this.id = id;
      this.bbox = bbox;
      this.title = title;
    }

    public String getId()
    {
      return id;
    }

    public String getTitle()
    {
      return title;
    }

    public GM_Envelope getEnvelope()
    {
      return bbox;
    }

  }
}