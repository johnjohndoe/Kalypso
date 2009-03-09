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
package org.kalypso.ogc.gml.serialize;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCoreDebug;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gml.GMLException;
import org.kalypso.gml.GMLWorkspaceInputSource;
import org.kalypso.gml.GMLWorkspaceReader;
import org.kalypso.gml.GMLorExceptionContentHandler;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;

/**
 * Helper - Klasse, um Gml zu lesen und zu schreiben.
 * 
 * @author Gernot Belger
 */
public final class GmlSerializer
{
  public final static IFeatureProviderFactory DEFAULT_FACTORY = new GmlSerializerFeatureProviderFactory();

  private GmlSerializer( )
  {
    // do not instantiate this class
  }

  public static void serializeWorkspace( final OutputStreamWriter writer, final GMLWorkspace workspace ) throws GmlSerializeException
  {
    serializeWorkspace( writer, workspace, writer.getEncoding() );
  }

  /**
   * Writes a {@link GMLWorkspace} into an {@link File}. If the filename ends with <code>.gz</code>, the output will be
   * compressed with GZip.
   */
  public static void serializeWorkspace( final File gmlFile, final GMLWorkspace gmlWorkspace, final String encoding ) throws IOException, GmlSerializeException
  {
    OutputStream os = null;
    try
    {
      final BufferedOutputStream bs = new BufferedOutputStream( new FileOutputStream( gmlFile ) );

      // REMARK: this is a quite crude way to decide, if to compress or not. But how should we decide it anyway?
      if( gmlFile.getName().endsWith( ".gz" ) )
      {
        os = new GZIPOutputStream( bs );
      }
      else
      {
        os = bs;
      }

      GmlSerializer.serializeWorkspace( os, gmlWorkspace, encoding );
      os.close();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  /**
   * REMARK: This method closes the given writer, which is VERY bad. Every caller should close the write on its own
   * 
   * @deprecated Because this method closes it writer. Change to {@link #serializeWorkspace(Writer, GMLWorkspace,
   *             String, false)}, rewrite your code, then we can get rid of this method and the flag.
   */
  @Deprecated
  public static void serializeWorkspace( final Writer writer, final GMLWorkspace gmlWorkspace, final String charsetEncoding ) throws GmlSerializeException
  {
    serializeWorkspace( writer, gmlWorkspace, charsetEncoding, true );
  }

  public static void serializeWorkspace( final Writer writer, final GMLWorkspace gmlWorkspace, final String charsetEncoding, final boolean closeWriter ) throws GmlSerializeException
  {
    final GMLWorkspaceInputSource inputSource = new GMLWorkspaceInputSource( gmlWorkspace );
    final StreamResult result = new StreamResult( writer );

    try
    {
      serializeWorkspace( inputSource, result, charsetEncoding );
    }
    finally
    {
      if( closeWriter )
      {
        IOUtils.closeQuietly( writer );
      }
    }
  }

  public static void serializeWorkspace( final OutputStream is, final GMLWorkspace gmlWorkspace, final String charsetEncoding ) throws GmlSerializeException
  {
    final GMLWorkspaceInputSource inputSource = new GMLWorkspaceInputSource( gmlWorkspace );
    final StreamResult result = new StreamResult( is );

    serializeWorkspace( inputSource, result, charsetEncoding );
  }

  /**
   * Most general way to serialize the workspace. All other serialize methods should eventually cal this method.
   */
  public static void serializeWorkspace( final GMLWorkspaceInputSource inputSource, final StreamResult result, final String charsetEncoding ) throws TransformerFactoryConfigurationError, GmlSerializeException
  {
    try
    {
      final XMLReader reader = new GMLWorkspaceReader();
      reader.setFeature( "http://xml.org/sax/features/namespaces", true ); //$NON-NLS-1$
      reader.setFeature( "http://xml.org/sax/features/namespace-prefixes", true ); //$NON-NLS-1$

      final Source source = new SAXSource( reader, inputSource );

      final TransformerFactory tFac = TransformerFactory.newInstance();
      final Transformer transformer = tFac.newTransformer();
      transformer.setOutputProperty( OutputKeys.ENCODING, charsetEncoding );
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "1" );
      transformer.setOutputProperty( OutputKeys.METHOD, "xml" ); //$NON-NLS-1$
      // TODO: maybe also use OutputKeys.CDATA_SECTION_ELEMENTS ? See the marshallMethod of the XSDBaseTypeHandlerString
      // TODO put new QName( NS.OM, "result" ) here instead inside the GMLSaxFactory
      // Problem: must now know the prefix of NS.OM
      transformer.transform( source, result );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.4" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * @param idMap
   *          (existing-ID,new-ID) mapping for ids, replace all given Ids in GML (feature-ID and links)
   */
  public static void serializeWorkspace( final Writer writer, final GMLWorkspace gmlWorkspace, final String charsetEncoding, final Map<String, String> idMap ) throws GmlSerializeException
  {
    try
    {
      final XMLReader reader = new GMLWorkspaceReader();
      reader.setFeature( "http://xml.org/sax/features/namespaces", true ); //$NON-NLS-1$
      reader.setFeature( "http://xml.org/sax/features/namespace-prefixes", true ); //$NON-NLS-1$

      final InputSource inputSource = new GMLWorkspaceInputSource( gmlWorkspace );
      inputSource.setEncoding( charsetEncoding );

      final Source source = new SAXSource( reader, inputSource );

      // TODO: change to stream instead of writer
      final StreamResult result = new StreamResult( writer );

      final TransformerFactory tFac = TransformerFactory.newInstance();
      final Transformer transformer = tFac.newTransformer();
      transformer.setOutputProperty( OutputKeys.ENCODING, charsetEncoding );
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "1" );
      transformer.setOutputProperty( OutputKeys.METHOD, "xml" ); //$NON-NLS-1$
      // TODO: maybe also use OutputKeys.CDATA_SECTION_ELEMENTS ? See the marshallMethod of the XSDBaseTypeHandlerString
      transformer.transform( source, result );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.4" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * Same as {@link #createGMLWorkspace(URL, IFeatureProviderFactory, null )
   */
  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IFeatureProviderFactory factory ) throws Exception
  {
    return createGMLWorkspace( gmlURL, factory, null );
  }

  /**
   * Reads a {@link GMLWorkspace} from the contents of an {@link URL}.
   */
  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IFeatureProviderFactory factory, final IProgressMonitor monitor ) throws Exception
  {
    InputStream is = null;
    InputStream urlStream = null;
    try
    {
      urlStream = gmlURL.openStream();

      InputStream bis;
      if( monitor == null )
      {
        bis = new BufferedInputStream( urlStream );
      }
      else
      {
        final long contentLength = getContentLength( gmlURL );
        final String tskMsg = String.format( "Reading <%s>", gmlURL );
        monitor.beginTask( tskMsg, (int) contentLength );
        bis = new ProgressInputStream( urlStream, contentLength, monitor );
      }

      if( gmlURL.toExternalForm().endsWith( ".gz" ) )
      {
        is = new GZIPInputStream( bis );
      }
      else
      {
        is = bis;
      }

      final GMLWorkspace workspace = createGMLWorkspace( new InputSource( is ), null, gmlURL, factory );
      is.close();
      return workspace;
    }
    catch( final IOException e )
    {
      // Handle cancel of progress monitor: ProgressInputStreams throws IOException with a CoreException as cause
      if( e == ProgressInputStream.CANCEL_EXCEPTION )
        throw new CoreException( StatusUtilities.createStatus( IStatus.CANCEL, "Canceled - Serializing of GML Workspace", e ) );

      throw e;
    }
    finally
    {
      IOUtils.closeQuietly( is );
      // also close <code>bis</code> separately, as GZipInputStream throws exception in constructor
      IOUtils.closeQuietly( urlStream );
      if( monitor != null )
      {
        monitor.done();
      }
    }
  }

  private static long getContentLength( final URL url ) throws IOException
  {
    final File file = FileUtils.toFile( url );
    if( file != null )
      return file.length();

    final File platformFile = ResourceUtilities.findJavaFileFromURL( url );
    if( platformFile != null )
      return platformFile.length();

    final URLConnection connection = url.openConnection();
    final int contentLength = connection.getContentLength();
    return contentLength;
  }

  public static GMLWorkspace createGMLWorkspace( final InputSource inputSource, final URL schemaLocationHint, final URL context, final IFeatureProviderFactory factory ) throws ParserConfigurationException, SAXException, SAXNotRecognizedException, SAXNotSupportedException, IOException, GMLException
  {
    TimeLogger perfLogger = null;
    if( KalypsoCoreDebug.PERF_SERIALIZE_GML.isEnabled() )
    {
      perfLogger = new TimeLogger( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.7" ) ); //$NON-NLS-1$
    }

    final IFeatureProviderFactory providerFactory = factory == null ? DEFAULT_FACTORY : factory;

    final SAXParserFactory saxFac = SAXParserFactory.newInstance();
    saxFac.setNamespaceAware( true );

    final SAXParser saxParser = saxFac.newSAXParser();
    // make namespace-prefixes visible to content handler
    // used to allow necessary schemas from gml document
    final XMLReader xmlReader = saxParser.getXMLReader();
    xmlReader.setFeature( "http://xml.org/sax/features/namespace-prefixes", Boolean.TRUE ); //$NON-NLS-1$

    // TODO: also set an error handler here

    final GMLorExceptionContentHandler exceptionHandler = new GMLorExceptionContentHandler( xmlReader, schemaLocationHint, context, providerFactory );
    xmlReader.setContentHandler( exceptionHandler );
    xmlReader.parse( inputSource );

    final GMLWorkspace workspace = exceptionHandler.getWorkspace();

    if( perfLogger != null )
    {
      perfLogger.takeInterimTime();
      perfLogger.printCurrentTotal( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.8" ) ); //$NON-NLS-1$
    }

    return workspace;
  }

  public static GMLWorkspace createGMLWorkspace( final File file, final IFeatureProviderFactory factory ) throws Exception
  {
    InputStream is = null;
    BufferedInputStream bis = null;

    try
    {
      bis = new BufferedInputStream( new FileInputStream( file ) );

      if( file.getName().endsWith( ".gz" ) )
      {
        is = new GZIPInputStream( bis );
      }
      else
      {
        is = bis;
      }

      final GMLWorkspace workspace = createGMLWorkspace( is, null, factory );
      is.close();
      return workspace;
    }
    finally
    {
      IOUtils.closeQuietly( is );
      // Close <code>bis<code> separately, as GZipInputStream throws exception constructor
      IOUtils.closeQuietly( bis );
    }

  }

  public static GMLWorkspace createGMLWorkspace( final InputStream inputStream, final URL schemaURLHint, final IFeatureProviderFactory factory ) throws Exception
  {
    return createGMLWorkspace( new InputSource( inputStream ), schemaURLHint, null, factory );
  }

  /**
   * Creates an (empty) gml file, containing only one root feature of a given type.
   */
  public static void createGmlFile( final IFeatureType rootFeatureType, final IFile targetFile, final IProgressMonitor monitor, final IFeatureProviderFactory factory ) throws CoreException
  {
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.10" ), 2 ); //$NON-NLS-1$
      final IFeatureProviderFactory providerFactory = factory == null ? DEFAULT_FACTORY : factory;
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( rootFeatureType, ResourceUtilities.createURL( targetFile ), providerFactory );
      monitor.worked( 1 );

      final SetContentHelper contentHelper = new SetContentHelper()
      {
        @Override
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          GmlSerializer.serializeWorkspace( writer, workspace );
        }
      };
      contentHelper.setFileContents( targetFile, false, true, new SubProgressMonitor( monitor, 1 ) );
      monitor.worked( 1 );
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  public static void createGmlFile( final QName rootFeatureQName, final String[] introduceNamespaces, final IFile targetFile, final IProgressMonitor monitor, final IFeatureProviderFactory factory ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ogc.gml.serialize.GmlSerializer.11" ), 2 ); //$NON-NLS-1$

    final IFeatureProviderFactory providerFactory = factory == null ? DEFAULT_FACTORY : factory;

    URL context = null;
    try
    {
      context = targetFile.getLocationURI().toURL();
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( rootFeatureQName, context, providerFactory );

    // introduce further schemata into workspace
    final IGMLSchema schema = workspace.getGMLSchema();
    if( introduceNamespaces != null && schema instanceof GMLSchema )
    {
      final GMLSchema gmlSchema = (GMLSchema) schema;
      for( final String namespaceUri : introduceNamespaces )
      {
        try
        {
          gmlSchema.getGMLSchemaForNamespaceURI( namespaceUri );
        }
        catch( final GMLSchemaException e )
        {
          // probably not a vital error, just log it
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoCorePlugin.getDefault().getLog().log( status );
        }
      }
    }

    monitor.worked( 1 );

    final SetContentHelper contentHelper = new SetContentHelper()
    {
      @Override
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        GmlSerializer.serializeWorkspace( writer, workspace );
      }
    };
    contentHelper.setFileContents( targetFile, false, true, new SubProgressMonitor( monitor, 1 ) );
    monitor.worked( 1 );
  }

  /**
   * This function loads a workspace from a {@link IFile}.
   * 
   * @param file
   *          The file of the workspace.
   * @return The workspace of the file.
   */
  public static GMLWorkspace createGMLWorkspace( final IFile file ) throws Exception
  {
    /* Create the url of the workspace. */
    final URL url = ResourceUtilities.createURL( file );

    /* Load the workspace and return it. */
    return GmlSerializer.createGMLWorkspace( url, null );
  }

  /**
   * This function saves a given workspace to a file. Don't forget to set your charset to the file you are about to
   * create. It will be used by this function.
   * 
   * @param workspace
   *          The workspace to save.
   * @param file
   *          The file to save the workspace to. <strong>Note:</strong> The file must point to a real file.
   */
  public static void saveWorkspace( final GMLWorkspace workspace, final IFile file ) throws Exception
  {
    if( workspace == null || file == null )
      throw new Exception( "Either the workspace or the target file was null." );

    /* The default encoding is that of the file. */
    final String encoding = file.getCharset();

    /* Create a writer. */
    final File javaFile = file.getLocation().toFile();

    /* Save the workspace. */
    serializeWorkspace( javaFile, workspace, encoding );

    /* Refresh the file. */
    file.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
  }

  /**
   * serializes a workspace into a zipfile
   */
  // TODO: grrr, do not use fixed CP1252 here!!! And do not use writers as well
  public static void serializeWorkspaceToZipFile( final File gmlZipResultFile, final GMLWorkspace resultWorkspace, final String zipEntryName ) throws FileNotFoundException
  {
    final ZipOutputStream zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( gmlZipResultFile ) ) );
    try
    {
      final ZipEntry newEntry = new ZipEntry( zipEntryName );
      zos.putNextEntry( newEntry );
      final OutputStreamWriter gmlWriter = new OutputStreamWriter( zos, "CP1252" );

      serializeWorkspace( gmlWriter, resultWorkspace, "CP1252", new HashMap<String, String>() );

      zos.closeEntry();
      zos.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( zos );
    }
  }
}