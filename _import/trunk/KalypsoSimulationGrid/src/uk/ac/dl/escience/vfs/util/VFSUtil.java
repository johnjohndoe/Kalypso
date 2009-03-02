/*
 * VFSUtil.java
 *
 * Created on 16 May 2007, 09:48
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package uk.ac.dl.escience.vfs.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.vfs.AllFileSelector;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpClientFactory;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpFileObject;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpFileSystem;
import org.globus.ftp.GridFTPClient;
import org.globus.ftp.MarkerListener;
import org.gridforum.jgss.ExtendedGSSCredential;
import org.gridforum.jgss.ExtendedGSSManager;
import org.ietf.jgss.GSSCredential;
import org.ietf.jgss.GSSException;

/**
 * @author David Meredith Class used to perform recursive copying between different file systems. For two gridftp uri's,
 *         you can perform third party file transfers which means bytes do not have to be piped through the VFS client.
 */
public class VFSUtil
{

  private static Log log = LogFactory.getLog( VFSUtil.class );

  public static GSSCredential loadProxy( String location ) throws FileNotFoundException, IOException, GSSException
  {
    return loadProxy( new File( location ) );
  }

  public static GSSCredential loadProxy( File location ) throws FileNotFoundException, IOException, GSSException
  {
    byte[] data = new byte[(int) location.length()];
    FileInputStream in = new FileInputStream( location );// read in the credential data
    in.read( data );
    in.close();

    ExtendedGSSManager manager = (ExtendedGSSManager) ExtendedGSSManager.getInstance();
    GSSCredential cred = manager.createCredential( data, ExtendedGSSCredential.IMPEXP_OPAQUE, GSSCredential.DEFAULT_LIFETIME, null, GSSCredential.INITIATE_AND_ACCEPT );
    return cred;
  }

  /*
   * public static void testFileSystemExceptionMessageOK() throws FileSystemException{ if(true) throw new
   * FileSystemException("vfs.provider/copy-missing-file.error"); }
   */

  /**
   * Copy the content of srcFo to destFo.
   * <p/>
   * 1) if srcFo is a file and destFo is a dir, then copy srcFo INTO destTo. 2) if srcFo is a file and destFo is a file,
   * then OVERWRITE destFo. 3) if srcFo is a file and destFo is IMAGINARY, then create destFo file. 4) if srcFo is a dir
   * and destFo is a dir, then copy children of srcFo INTO destFo (*) 5) if srcFo is a dir and destFo is IMAGINARY, then
   * create destFo dir. 6) if srcFo is a dir and destFo is a file, then throw IOException
   * <p>
   * Copying the children of the srcFo directory into the destFo dir is preferable to copying the srcFo dir inclusive
   * into the destFo dir. This is because the user may want to provide an alternative base name for the destFo dir (e.g.
   * 'dirAcopy' rather than 'dirA'). Do the following: destFo = obj.resolveFile("newDirName"); if(!destFo.exists())
   * destFo.createFolder();
   * </p>
   * 
   * @param srcFo
   *          the source file to copy from
   * @param destFo
   *          the destination file to create/overwrite
   * @param doThirdPartyTransferForTwoGridFtpFileObjects
   *          If true and both srcFo and destFo are gsiftp uris, then attempt a gridftp third party file transfer
   *          (requires cog.properties), otherwise do byte IO (byte streaming) between srcFo and destFo.
   * @throws IOException
   *           if an error occurs on byte transfer
   * @throws org.apache.commons.vfs.FileSystemException
   *           if srcFo does not exist, if srcFo is a directory and destFo exists and is a file, if destFo is not
   *           writable
   */
  public static void copy( FileObject srcFo, FileObject destFo, MarkerListener listener, boolean doThirdPartyTransferForTwoGridFtpFileObjects ) throws IOException, FileSystemException
  {

    // todo: support append
    // check srcFo file exsits and is readable
    if( !srcFo.exists() )
    {
      throw new FileSystemException( "vfs.provider/copy-missing-file.error", srcFo );
    }
    if( !srcFo.isReadable() )
    {
      throw new FileSystemException( "vfs.provider/read-not-readable.error", srcFo );
    }

    // create the destination file or folder if it does not already exist.
    if( destFo.getType() == FileType.IMAGINARY || !destFo.exists() )
    {
      if( srcFo.getType().equals( FileType.FILE ) )
      {
        destFo.createFile();
      }
      else if( srcFo.getType().equals( FileType.FOLDER ) )
      {
        destFo.createFolder();
      }
    }

    // check can write to the target
    if( !destFo.isWriteable() )
    {
      throw new FileSystemException( "vfs.provider/copy-read-only.error", destFo );
    }

    // check src and target FileObjects are not the same file
    if( destFo.getName().getURI().equals( srcFo.getName().getURI() ) )
    {
      throw new FileSystemException( "vfs.provider/copy-file.error", new Object[] { srcFo, destFo }, null );
    }

    // Do transfer
    // If two gsiftp uris and electing to do third party transfer
    if( doThirdPartyTransferForTwoGridFtpFileObjects && srcFo.getName().getScheme().equalsIgnoreCase( "gridftp" ) && destFo.getName().getScheme().equalsIgnoreCase( "gridftp" ) )
    {
      final GsiFtpFileSystem srcFS = (GsiFtpFileSystem) srcFo.getFileSystem();
      final GsiFtpFileSystem destFS = (GsiFtpFileSystem) destFo.getFileSystem();
      GridFTPClient srcClient = null;
      GridFTPClient destClient = null;
      // create a new GridFTPUtil as this class is not stateless !
      GridFTPUtil gridftpUtil = new GridFTPUtil();
      try
      {
        srcClient = srcFS.getClient();
        gridftpUtil.setSourceClient( srcClient );
        gridftpUtil.setMarkerListener( listener );
        if( srcFS == destFS )
        {
          // need to create new client to same host
          destClient = GsiFtpClientFactory.createConnection( srcClient.getHost(), srcClient.getPort(), null );
          gridftpUtil.setDestClient( destClient );
          doGridFtpThridPartyTransfer( gridftpUtil, srcFo, destFo, false );
          destClient.close();
        }
        else
        {
          destClient = destFS.getClient();
          try
          {
            gridftpUtil.setDestClient( destClient );
            doGridFtpThridPartyTransfer( gridftpUtil, srcFo, destFo, false );
          }
          finally
          {
            if( destClient != null )
              destFS.putClient( destClient );
          }
        }
      }
      catch( final IOException e )
      {
        throw e;
      }
      catch( final Exception e )
      {
        throw new IOException( "Problem with 3rd-party transfer.", e );
      }
      finally
      {
        if( srcClient != null )
          srcFS.putClient( srcClient );
      }

      // Need to manually refresh the content here after the copy because
      // are not using the vfs approach which would normally cause re resolveFile
      final GsiFtpFileObject destGridFtpFO = (GsiFtpFileObject) destFo;
      destGridFtpFO.getInfo( true );

      return;
    }

    // Do transfer
    // If copying between two different file systems
    if( srcFo.getType().equals( FileType.FILE ) )
    {
      if( destFo.getType().equals( FileType.FOLDER ) )
      {
        log.debug( "vfs FILE into FOLDER" );
        // get a handle on the new file to create at the destination.
        FileObject nestedDestFo = destFo.resolveFile( srcFo.getName().getBaseName() );
        // copyFileToFile(srcFo, nestedDestFo, false); //append false here
        nestedDestFo.copyFrom( srcFo, new AllFileSelector() );

      }
      else
      {
        log.debug( "vfs FILE to FILE" );
        // copyFileToFile(srcFo, destFo, false); //append false here
        destFo.copyFrom( srcFo, new AllFileSelector() );

      }
    }
    else if( srcFo.getType().equals( FileType.FOLDER ) )
    {
      // copying the children of a folder into another folder
      if( destFo.getType().equals( FileType.FOLDER ) )
      {
        log.debug( "vfs FOLDER children into FOLDER" );
        destFo.copyFrom( srcFo, new AllFileSelector() );

      }
      else
      {
        throw new IOException( "Cannot copy a folder to a destination that is not a folder" );
      }
    }
    else
    {
      throw new IOException( "Cannot copy from path of type " + srcFo.getType() + " to another path of type " + destFo.getType() );
    }
  }

  public static void doGridFtpThridPartyTransfer( GridFTPUtil gridftpUtil, FileObject srcFO, FileObject destFO, boolean append ) throws Exception
  {

    final GridFTPClient srcClient = gridftpUtil.getSourceClient();
    final GridFTPClient destClient = gridftpUtil.getDestinationClient();

    if( srcFO.getType().equals( FileType.FILE ) )
    {
      if( destFO.getType().equals( FileType.FOLDER ) )
      {
        // copying a file into a directory
        log.debug( "gsiftp FILE into FOLDER" );
        String srcFullpathFile = srcFO.getName().getPath();
        // String destFullpathFile = pTo.getName().getPath() + "/" + pFrom.getName().getBaseName();
        FileObject newFile = destFO.resolveFile( srcFO.getName().getBaseName() );
        String destFullpathFile = newFile.getName().getPath();
        gridftpUtil.thirdPartyFileTransfer( srcClient, srcFullpathFile, destClient, destFullpathFile, append );

      }
      else
      {
        log.debug( "gsiftp FILE to FILE" );
        // copying a file to file (overwrite file or create if file is IMAGINARY)
        String srcFullpathFile = srcFO.getName().getPath();
        String destFullpathFile = destFO.getName().getPath();
        gridftpUtil.thirdPartyFileTransfer( srcClient, srcFullpathFile, destClient, destFullpathFile, append );

      }
    }
    else if( srcFO.getType().equals( FileType.FOLDER ) )
    {
      if( destFO.getType().equals( FileType.FOLDER ) )
      {
        // copy children of pFrom into pTo folder
        log.debug( "gsiftp FOLDER children into FOLDER" );
        String srcFullpathFile = srcFO.getName().getPath();
        String destDir = destFO.getName().getPath();
        gridftpUtil.thirdPartyTransferDir2( srcClient, srcFullpathFile, destClient, destDir, append );

      }
      else
      {
        throw new IOException( "cannot copy a folder to a destination that is not a folder" );
      }
    }
    else
    {
      throw new IOException( "cannot copy from path of type " + srcFO.getType() + " to another path of type " + destFO.getType() );
    }
  }

  /**
   * Copy the content of a virtual file from pFrom to another (optionally append the file content to the end of the pTo
   * file) Note. Not all file systems supports the append operation.
   * 
   * @param fromFo
   *          the file to read from
   * @param toFo
   *          the file to append to
   * @param append
   * @throws IOException
   *           error during the transfer
   */
  protected static void copyFileToFile( FileObject fromFo, FileObject toFo, boolean append ) throws IOException
  {
    if( !fromFo.exists() )
    {
      throw new IOException( "path " + fromFo + " is not found" );
    }
    if( !fromFo.getType().equals( FileType.FILE ) )
    {
      throw new IOException( "path " + fromFo + " is not a file" );
    }

    InputStream xIS = null;
    OutputStream xOS = null;
    try
    {
      xIS = fromFo.getContent().getInputStream();
      xOS = toFo.getContent().getOutputStream( append );
      byte xBytes[] = new byte[1024];
      int xByteRead = -1;
      int xTotalByteWrite = 0;
      while( (xByteRead = xIS.read( xBytes )) != -1 )
      {
        xOS.write( xBytes, 0, xByteRead );
        xTotalByteWrite = xTotalByteWrite + xByteRead;
      }
      log.info( "total byte write " + xTotalByteWrite );
      xOS.flush();
      xOS.flush();
    }
    catch( Exception xEx )
    {
      log.error( xEx.getMessage(), xEx );
      throw new IOException( xEx.getMessage() );
    }
    finally
    {
      try
      {
        fromFo.getContent().close();
      }
      catch( Exception xEx )
      {
      }
      try
      {
        toFo.getContent().close();
      }
      catch( Exception xEx )
      {
      }
      try
      {
        if( xIS != null )
        {
          xIS.close();
        }
      }
      catch( Exception ex )
      {
      }
      try
      {
        if( xOS != null )
        {
          xOS.close();
        }
      }
      catch( Exception ex )
      {
      }
    }
  }

}
