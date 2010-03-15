// MfcHelper.h: Schnittstelle für die Klasse MfcHelper.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MFCHELPER_H__EE941A91_587F_11D8_B4C2_00104BB3E525__INCLUDED_)
#define AFX_MFCHELPER_H__EE941A91_587F_11D8_B4C2_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

namespace BCE
{
  /**
   * static HelperFuntions for MFC
   */
  namespace MfcHelper
  {
    /**
     * Tries to create a releative Path to a given base-path.
     *
     * @param baseDir This is the baseDir, to which the rleative path is created
     * @param absolutePath This is the given absolute path
     *
     * @return if absolutePath points to a file or subdirectory below baseDir, the relative path corresponding to 
     *          baseDir and absolutePath is returned. If not, absolutePath is returned
     *         if absolutePath already is relative, it also is returned as is
     */
    CString GetPathAsRelative( const CString& baseDir, const CString& absolutePath );

    /** Combines a relative path and a baseDir. 
     *  If the relative Path is really absolute, it is returned unchainged.
     */
    CString CreateAbsolutePath( const CString& baseDir, const CString& relativePath );

    /** Returns the parent directory of the given path */
    CString GetFileDirectory( const CString& path );

    /** Return the FileName of the given path */
    CString GetFileName( const CString& path );

    /** Returns the file-extension of the given path */
    CString GetFileExt( const CString& path );

    /** Returns the path without the extension */
    CString GetFileWOExt( const CString& path );

    /** Returns the file-title = filename without extension of the given path */
    CString GetFileTitle( const CString& path );

    /** Returns the drive name of the given path */
    CString GetFileDrive(const CString& path);

    /** 
     * erzeugt ( notfalls durch anhängen von Nummern ) einen unbenutzten Dateinamen
     * 
     * @param base Pfad + Basisname des zu erzeugenden Dateinamens
     * @param ext Erweiterung des Dateinamens
     *
     * @return einen freien Dateinamen, ohne die Endung
     *
     * Bemerkung: es wird ein Dateiname der Form base + 'count' + ext erzeugt
     */
    CString GetUnusedFileName( const CString& base, const CString& ext, const BOOL makeShpConform = TRUE );
  };
};

#endif // !defined(AFX_MFCHELPER_H__EE941A91_587F_11D8_B4C2_00104BB3E525__INCLUDED_)
