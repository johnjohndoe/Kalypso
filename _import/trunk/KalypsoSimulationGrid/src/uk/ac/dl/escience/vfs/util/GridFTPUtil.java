package uk.ac.dl.escience.vfs.util;

//import uk.ac.dl.esience.vfs.util.MarkerListenerImpl;
//import uk.ac.dl.esience.vfs.util.FileAttributes;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;

import org.globus.ftp.FileInfo;
import org.globus.ftp.GridFTPClient;
import org.globus.ftp.GridFTPSession;
import org.globus.ftp.HostPortList;
import org.globus.ftp.MarkerListener;
import org.globus.ftp.RetrieveOptions;
import org.globus.ftp.exception.ServerException;

/**
 * @author Xiaobo Yang
 * @author David Meredith
 */
public class GridFTPUtil
{

  private final String SEP = "/";

  private long fileSize = -1;

  MarkerListener markerListener;

  private GridFTPClient sourceClient;

  private GridFTPClient destClient;

  /** Creates a new instance of GridFTPUtil */
  public GridFTPUtil( )
  {
    markerListener = null;
    sourceClient = null;
    destClient = null;
  }

  public void close( ) throws IOException, ServerException
  {
    if( sourceClient != null )
      sourceClient.close();

    if( destClient != null )
      destClient.close();
  }

  public void setMarkerListener( MarkerListener listener )
  {
    markerListener = listener;
  }

  public GridFTPClient getDestinationClient( )
  {
    return destClient;
  }

  public GridFTPClient getSourceClient( )
  {
    return sourceClient;
  }

  public void setDestClient( GridFTPClient _destClient )
  {
    destClient = _destClient;
  }

  public void setSourceClient( GridFTPClient _sourceClient )
  {
    sourceClient = _sourceClient;
  }

  public void abort( )
  {
    try
    {
      sourceClient.abort();
      destClient.abort();
      close();
      close();
    }
    catch( Exception e )
    {
      System.err.println( "error while aboring transfer: " + e.toString() );
      e.printStackTrace();
    }

  }

  /**
   * @param path
   *          - absolute path or relative path from current direcotry of the directory you want to list.
   */
  public Vector getDirInfo( GridFTPClient client, String path ) throws Exception
  {
    Vector v = null;
    try
    {
      String currDir = client.getCurrentDir();

      if( path == null )
        path = "";
      if( path.equals( "" ) )
        client.changeDir( currDir + SEP + path );
      else
        client.changeDir( path );

      currDir = client.getCurrentDir();

      //
      // Xiaobo Yang, 10 November 2004
      // In case this method is called more than one times
      //
      client.setPassive();
      client.setLocalActive();

      v = client.list();

      // System.out.println("[GridFTPUtil] getDirInfo() finished");
    }
    catch( Exception e )
    {
      System.out.println( "[GridFTPUtil] Exception caught - " + e.toString() );
    }

    return v;
  }

  /**
   * Xiaobo Yang, 3 December 2004 3rd-party transfer, transfer directory
   * 
   * @param srcDir
   *          - absolute full path of directory you want to transfer on sourcehost
   * @param destDir
   *          - absolute full path of either an IMAGINARY directory you want to create via the transfer or an existing
   *          directory on destination host. Note, you can re-name the directory name, e.g. this is ok: srcDir =
   *          "/home/dave/dir" destDir = "/home/djm76/dirCopied"
   */
  public void thirdPartyTransferDir2( GridFTPClient srcGFC, String srcDir, GridFTPClient destGFC, String destDir, boolean b_append ) throws Exception
  {

    // if(srcDir.equals(".") || srcDir.equals("..") || destDir.equals(".") || destDir.equals(".."))
    // return;

    if( !destGFC.exists( destDir ) ) // if dir_dest does not exist
      destGFC.makeDir( destDir ); // create destDir on destHost

    destGFC.changeDir( destDir ); // set destDir as current directory

    // get a list of all files & directories of srcDir on srcHost
    Vector vSrcDirInfo = getDirInfo( srcGFC, srcDir );
    Enumeration enumeration = vSrcDirInfo.elements();
    while( enumeration.hasMoreElements() )
    {
      FileInfo finfo = (FileInfo) enumeration.nextElement();
      setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
      // System.out.println("recurse: "+finfo.getName());

      if( finfo.isDirectory() && finfo.getName().equals( "." ) )
      {
        // exclude dot //System.out.println("dot");
      }
      else if( finfo.isDirectory() && finfo.getName().equals( ".." ) )
      {
        // exclude dot dot System.out.println("dot dot");
      }
      else if( finfo.isDirectory() )
      { // directory !finfo.getName().startsWith("."))
        // System.out.println("recurse dir: "+finfo.getName());
        thirdPartyTransferDir2( srcGFC, srcDir + SEP + finfo.getName(), destGFC, destDir + SEP + finfo.getName(), b_append );
      }
      else if( finfo.isFile() )
      { // file
        // System.out.println("transfering: "+finfo.getName());
        try
        {
          thirdPartyFileTransfer( srcGFC, srcDir, finfo.getName(), destGFC, destDir, finfo.getName(), b_append );
        }
        catch( InterruptedException ie )
        {
          System.err.println( "Intercept encountered" );
        }

      }
      /*
       * else if (finfo.isSoftLink()) { // softlink } else if (finfo.isDevice()) { // device }
       */
    }

    setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
  }

  /**
   * Xiaobo Yang, Dave Meredith 3rd party transfer file if file > 1MB transfer in parallel mode, otherwise in
   * non-parallel mode
   * 
   * @param srcFullpathFile
   *          - absolute full path of the source file (must start with "/")
   * @param destFullpathFile
   *          - absolute full path of the dest file (must start with "/")
   */
  public void thirdPartyFileTransfer( GridFTPClient srcGFC, String srcFullpathFile, GridFTPClient destGFC, String destFullpathFile, boolean b_append ) throws Exception
  {

    this.sourceClient = srcGFC;
    this.destClient = destGFC;

    if( !srcFullpathFile.startsWith( SEP ) )
    {
      fileSize = -1;
      return;
    }
    if( !destFullpathFile.startsWith( SEP ) )
    {
      fileSize = -1;
      return;
    }
    if( !srcGFC.exists( srcFullpathFile ) )
    {
      fileSize = -1;
      return;
    }

    fileSize = srcGFC.getSize( srcFullpathFile );

    if( fileSize > 1048576 )
    { // file > 1MB transfer in parallel mode
      setParams3rdPartyParallelMode( srcGFC, destGFC );
      srcGFC.extendedTransfer( srcFullpathFile, destGFC, destFullpathFile, markerListener );
    }
    else
    { // file <= 1MB transfer in non-parallel mode
      setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
      srcGFC.transfer( srcFullpathFile, destGFC, destFullpathFile, b_append, null ); // 3rd party transfer
    }
    setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
  }

  /**
   * Xiaobo Yang, Dave Meredith 3rd party transfer file if file > 1MB transfer in parallel mode, otherwise in
   * non-parallel mode
   * 
   * @param srcDir
   *          - absolute full path of the source dir (must start with "/")
   * @param srcFile
   *          - the name of the file you want to transfer (just name, no path info)
   * @param destDir
   *          - absolute full path of the dest dir (must start with "/")
   * @param destFile
   *          - the name of the file on on the destination host (note, you can specify a new name for the file - just
   *          name, no path info)
   */
  public void thirdPartyFileTransfer( GridFTPClient srcGFC, String srcDir, String srcFile, GridFTPClient destGFC, String destDir, String destFile, boolean b_append ) throws Exception
  {

    if( !srcDir.startsWith( SEP ) )
    {
      fileSize = -1;
      return;
    }
    if( !destDir.startsWith( SEP ) )
    {
      fileSize = -1;
      return;
    }

    if( srcDir.trim().endsWith( SEP ) )
    {
      int endIndex = srcDir.length();
      srcDir = srcDir.substring( 0, endIndex - 1 );
    }
    if( destDir.trim().endsWith( SEP ) )
    {
      int endIndex = destDir.length();
      destDir = destDir.substring( 0, endIndex - 1 );
    }

    String srcFullpathFile = srcDir + SEP + srcFile;
    String destFullpathFile = destDir + SEP + destFile;

    if( !srcGFC.exists( srcFullpathFile ) )
    {
      fileSize = -1;
      return;
    }

    fileSize = srcGFC.getSize( srcFullpathFile );

    if( fileSize > 1048576 )
    { // file > 1MB transfer in parallel mode
      setParams3rdPartyParallelMode( srcGFC, destGFC );
      long t_start = System.currentTimeMillis();
      srcGFC.extendedTransfer( srcFullpathFile, destGFC, destFullpathFile,
      // null);
      markerListener );
      double dt = (double) (System.currentTimeMillis() - t_start) / 1000.0;
    }
    else
    { // file <= 1MB transfer in non-parallel mode
      setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
      srcGFC.transfer( srcFullpathFile, destGFC, destFullpathFile, b_append, null ); // 3rd party transfer
    }
    setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
    setParams3rdPartyBackToNonParallelMode( srcGFC, destGFC );
  }

  /**
   * Xiaobo Yang, 10 November 2004 Check whether "dir" is a directory on remote "host" Returns "true" if "dir" is a
   * directory, otherwise "false"
   * 
   * @param dir
   *          - absolute path or relative path from users home directory of the directory you want to check
   */
  public boolean checkDir( GridFTPClient gridFTPClient, String dir )
  {
    boolean b_isDir = false;
    try
    {
      gridFTPClient.changeDir( dir );
      b_isDir = true;
    }
    catch( IOException ioe )
    {
      b_isDir = false;
      System.out.println( "[GridFTPUtil] checkDir() java.io.IOException caught - " + ioe.toString() );
    }
    catch( ServerException se )
    {
      b_isDir = false;
      System.out.println( "[GridFTPUtil] checkDir() org.globus.ftp.exception.ServerException caught - " + se.toString() );
    }
    finally
    {
      return b_isDir;
    }

  }

  /**
   * @param fileDirPath
   *          - absolute path or relative path from users home directory of the file or directory you want to check if
   *          exists.
   */
  public boolean exists( GridFTPClient gridFTPClient, String fileDirPath )
  {
    boolean exists = false;
    try
    {
      exists = gridFTPClient.exists( fileDirPath );
    }
    catch( IOException ioe )
    {
      System.out.println( "[GridFTPUtil] fileDirExists() java.io.IOException caught - " + ioe.toString() );
    }
    catch( ServerException se )
    {
      System.out.println( "[GridFTPUtil] fileDirExists() org.globus.ftp.exception.ServerException caught - " + se.toString() );
    }
    return exists;
  }

  /**
   * Delete a remote directory (can delete both empty and not-empty dirs) Wrapper around deleteRemoteSubDir2.
   * 
   * @param absoluteDirPath
   *          - the absolute path of the directory you want to delete
   */
  public void deleteRemoteSubDir( GridFTPClient gridFTPClient, String absoluteDirPath ) throws Exception
  {

    if( absoluteDirPath == null )
      throw new IllegalArgumentException( "absoluteDirPath is null" );

    // remove leading/trailing whitespace
    absoluteDirPath = absoluteDirPath.trim();

    // remove the last separator '/' if present.
    while( absoluteDirPath.endsWith( "/" ) )
    {
      absoluteDirPath = absoluteDirPath.substring( 0, absoluteDirPath.length() - 1 );
    }
    int index = absoluteDirPath.lastIndexOf( "/" );
    if( index == -1 )
      return;

    // get the absolute path of the parent directory
    String dir_parent = absoluteDirPath.substring( 0, index );
    // get the name of the directory you want to delete (not path)
    String dir_name = absoluteDirPath.substring( index + 1, absoluteDirPath.length() );

    // if no valid dir to delete return;
    if( dir_parent == null || dir_name == null || dir_name.trim().length() <= 0 )
    {
      return;
    }

    /*
     * if(true) { System.out.println("Full Dir  :  "+absoluteDirPath); System.out.println("Parent Dir:  "+dir_parent);
     * System.out.println("Delete Dir:  "+dir_name); return; }
     */

    // client.deleteDir(absoluteDirPath); // will only delete an empty dir !
    this.deleteRemoteSubDir2( gridFTPClient, dir_parent, dir_name );

  }

  /**
   * Xiaobo Yang, David Meredith Delete directory on remote host
   * 
   * @param dir_parent
   *          - absolute full path of the parent directory (i.e. the parent directory of dir_sub)
   * @param dir_sub
   *          - the name of the directory you want to delete in dir_parent (note, this is just the 'name' of the
   *          directory, no path information).
   */
  public void deleteRemoteSubDir2( GridFTPClient gridFTPClient, String dir_parent, String dir_sub ) throws Exception
  {
    // get a list of all files & directories of dir on host
    Vector v_DirInfo = getDirInfo( gridFTPClient, dir_parent + SEP + dir_sub );

    // count all files and dirs except . and ..
    int totalNumber = 0;
    Enumeration en = v_DirInfo.elements();
    while( en.hasMoreElements() )
    {
      FileInfo finfo = (FileInfo) en.nextElement();
      if( finfo.isDirectory() && finfo.getName().equals( "." ) )
      {
        // do include dot
      }
      else if( finfo.isDirectory() && finfo.getName().equals( ".." ) )
      {
        // do not include dot dot
      }
      else
      {
        ++totalNumber;
      }
    }

    // int totalNumber = v_DirInfo.size();
    // System.out.println("tot: "+totalNumber);
    // if(true) return;

    if( totalNumber == 0 )
    { // dir on host is empty
      gridFTPClient.setLocalPassive();
      gridFTPClient.setActive();
      gridFTPClient.changeDir( dir_parent );
      gridFTPClient.deleteDir( dir_sub ); // dir is empty so delete it
    }
    else
    { // dir_sub is not empty - can contain hidden elems.
      Enumeration enumeration = v_DirInfo.elements(); // dir is not empty
      int i_subDir = 0;
      Vector<String> v_subDir = new Vector<String>();
      while( enumeration.hasMoreElements() )
      {
        FileInfo finfo = (FileInfo) enumeration.nextElement();
        gridFTPClient.setLocalPassive();
        gridFTPClient.setActive();

        if( finfo.isDirectory() && finfo.getName().equals( "." ) )
        {
          // do nothing with dot
        }
        else if( finfo.isDirectory() && finfo.getName().equals( ".." ) )
        {
          // do nothing with dot dot
        }
        else if( finfo.isDirectory() )
        { // sub-directory exists
          i_subDir++;
          v_subDir.add( finfo.getName() );
        }
        else
          gridFTPClient.deleteFile( finfo.getName() );
      }

      if( i_subDir > 0 )
      { // sub dir exists, delete
        for( int i = 0; i < i_subDir; i++ )
        {
          deleteRemoteSubDir2( gridFTPClient, dir_parent + SEP + dir_sub, (String) v_subDir.get( i ) );
        }
      }

      // now no sub dir, delete dir
      gridFTPClient.setLocalPassive();
      gridFTPClient.setActive();
      gridFTPClient.changeDir( dir_parent );
      gridFTPClient.deleteDir( dir_sub ); // dir is empty so delete it
    }
  }

  //
  // Xiaobo Yang, 3 December 2004
  // Methods setParams3rdParty(), setParams3rdPartyModeE(),
  // setParams3rdPartyParallelMode(), setParams3rdPartyBackToNonParallelMode()
  // are used to set correct server mode for 3rd party file transfers
  // using gridftp (i.e. parallel and striped transfers)
  // (note, not used for listing, checking directory, directory deletion etc).
  //
  // called for thirdparty file transfers from methods above.
  private void setParams3rdPartyParallelMode( GridFTPClient srcGFC, GridFTPClient destGFC ) throws Exception
  {
    setParams3rdPartyModeE( srcGFC );
    setParams3rdPartyModeE( destGFC );
    srcGFC.setOptions( new RetrieveOptions( 5 ) );
    HostPortList hpl = destGFC.setStripedPassive();
    srcGFC.setStripedActive( hpl );
  }

  // called for thirdparty file transfers from methods above.
  private void setParams3rdPartyBackToNonParallelMode( GridFTPClient srcGFC, GridFTPClient destGFC ) throws Exception
  {
    setParams3rdParty( srcGFC );
    setParams3rdParty( destGFC );
    destGFC.setActive( srcGFC.setPassive() );
    // the line below is necessary for method list() if the transfer mode is
    // changed from parallel to non-parallel
    srcGFC.setOptions( new RetrieveOptions( 1 ) );
  }

  // called only from above private methods (not called by public methods)
  private void setParams3rdPartyModeE( GridFTPClient client ) throws Exception
  {
    client.setProtectionBufferSize( 16384 );
    client.setType( GridFTPSession.TYPE_IMAGE );
    client.setMode( GridFTPSession.MODE_EBLOCK );
  }

  // called only from above private methods (not called by public methods)
  private void setParams3rdParty( GridFTPClient client ) throws Exception
  {
    client.setProtectionBufferSize( 16384 );
    client.setType( GridFTPSession.TYPE_IMAGE );
    client.setMode( GridFTPSession.MODE_STREAM );
  }
}

/*
 * public Vector<FileAttributes> toFileAttributes(Vector v, String imageLocation) { Vector dirV = new Vector();
 * Enumeration enumeration = v.elements(); while (enumeration.hasMoreElements()) { FileInfo finfo = (FileInfo)
 * enumeration.nextElement(); if (!finfo.isSoftLink()) { FileAttributes fattr = new FileAttributes(); try {
 * fattr.setName(java.net.URLEncoder.encode(finfo.getName(), "UTF-8")); } catch (UnsupportedEncodingException ue) {
 * fattr.setName(finfo.getName()); //System.out.println("[GridFTPUtil] UnsupportedEncodingException caught - " +
 * ue.toString()); } fattr.setTime(finfo.getTime() + " " + finfo.getDate()); if (finfo.isFile()) {
 * fattr.setSize(String.valueOf(finfo.getSize())); fattr.setisFile("1"); String filename = finfo.getName(); String ext =
 * ""; int idx = filename.lastIndexOf("."); if (idx >= 0) ext = filename.substring(idx + 1, filename.length()); if
 * (ext.equals("ps")) fattr.setPiclink(imageLocation + "ps.gif"); else if (ext.equalsIgnoreCase("pdf"))
 * fattr.setPiclink(imageLocation + "pdf.gif"); else if (ext.equalsIgnoreCase("zip")) fattr.setPiclink(imageLocation +
 * "zip.gif"); else if (ext.equalsIgnoreCase("jar")) fattr.setPiclink(imageLocation + "zip.gif"); else if
 * (ext.equalsIgnoreCase("gz")) fattr.setPiclink(imageLocation + "gz.gif"); else if (ext.equalsIgnoreCase("jpg"))
 * fattr.setPiclink(imageLocation + "image.gif"); else if (ext.equalsIgnoreCase("gif")) fattr.setPiclink(imageLocation +
 * "image.gif"); else if (ext.equalsIgnoreCase("tif")) fattr.setPiclink(imageLocation + "image.gif"); else if
 * (ext.equalsIgnoreCase("mpg")) fattr.setPiclink(imageLocation + "mpg.gif"); else if (ext.equalsIgnoreCase("mov"))
 * fattr.setPiclink(imageLocation + "mov.gif"); else if (ext.equalsIgnoreCase("htm")) fattr.setPiclink(imageLocation +
 * "web.gif"); else if (ext.equalsIgnoreCase("txt")) fattr.setPiclink(imageLocation + "text.gif"); else if
 * (ext.equalsIgnoreCase("java")) fattr.setPiclink(imageLocation + "java.gif"); else if (ext.equalsIgnoreCase("c"))
 * fattr.setPiclink(imageLocation + "c.gif"); else if (ext.equalsIgnoreCase("cpp")) fattr.setPiclink(imageLocation +
 * "cpp.gif"); else if (ext.equalsIgnoreCase("hpp")) fattr.setPiclink(imageLocation + "cpp.gif"); else if
 * (ext.equalsIgnoreCase("cc")) fattr.setPiclink(imageLocation + "cpp.gif"); else if (ext.equalsIgnoreCase("h"))
 * fattr.setPiclink(imageLocation + "h.gif"); else if (ext.equalsIgnoreCase("class")) fattr.setPiclink(imageLocation +
 * "class.gif"); else if (ext.equalsIgnoreCase("o")) fattr.setPiclink(imageLocation + "obj.gif"); else if
 * (ext.equalsIgnoreCase("obj")) fattr.setPiclink(imageLocation + "obj.gif"); else if (ext.equalsIgnoreCase("html"))
 * fattr.setPiclink(imageLocation + "web.gif"); else if (ext.equalsIgnoreCase("xml")) fattr.setPiclink(imageLocation +
 * "web.gif"); else if (ext.equalsIgnoreCase("doc")) fattr.setPiclink(imageLocation + "msword.gif"); else if
 * (ext.equalsIgnoreCase("ppt")) fattr.setPiclink(imageLocation + "ppt.gif"); else if (ext.equalsIgnoreCase("xls"))
 * fattr.setPiclink(imageLocation + "excel.gif"); else fattr.setPiclink(imageLocation + "file.gif"); } else if
 * (finfo.isDirectory()) { // starts fattr.setisFile("2"); // ends fattr.setPiclink(imageLocation + "folder.gif"); }
 * else fattr.setPiclink(imageLocation + "folder.gif"); dirV.add(fattr); } }
 * //System.out.println("[GridFTPUtil] toFileAttributes() finished"); return dirV; }
 */

/*
 * package uk.ac.dl.escience.vfs.util; public class FileAttributes implements java.io.Serializable { private String name
 * = ""; private String time = ""; private String size = ""; private String piclink = ""; private String isFile = "";
 * public FileAttributes() { } public void setName(String name) { this.name = name; } public String getName() { return
 * name; } public void setTime(String time) { this.time = time; } public String getTime() { return time; } public void
 * setSize(String size) { this.size = size; } public String getSize() { return size; } public void setPiclink(String
 * piclink) { this.piclink = piclink; } public String getPiclink() { return piclink; } public void setisFile(String
 * isFile) { this.isFile = isFile; } public String getisFile() { return isFile; } }
 */

