package org.kalypso.ogc.gml.serialize;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.tools.FeatureUtils;

/**
 * L�dt und schreibt ein CSV als {@link org.kalypsodeegree.model.feature.GMLWorkspace}.
 * 
 * Die Information, welche Spalte wie gelesen wird, wird per {@link #addInfo(FeatureTypeProperty, CSVInfo)}�bergeben.
 * 
 * @todo Einerseits ganz sch�n, genau zu spezifizieren, was die Spalten sind. Alternativ w�re aber auch super, wenn das
 *       auch automatisch anhand der 1.Zeile ginge
 * 
 * @todo Koordinatensystem ber�cksichtigen
 * 
 * @author belger
 */
public final class CsvFeatureReader
{
  public static final class CSVInfo
  {
    public final int[] columns;

    public final String format;

    public final boolean ignoreFormatExceptions;

    public CSVInfo( final String frmt, final int[] cols, final boolean ignoreFrmtExceptions )
    {
      this.format = frmt;
      this.columns = cols;
      this.ignoreFormatExceptions = ignoreFrmtExceptions;
    }
  }

  /** featureTypeProperty -> cvsinfo */
  private Map m_infos = new LinkedHashMap();

  public final void addInfo( final FeatureTypeProperty ftp, final CSVInfo info )
  {
    m_infos.put( ftp, info );
  }

  public final GMLWorkspace loadCSV( final Reader reader, final String comment, final String delemiter,
      final int lineskip ) throws IOException, CsvException
  {
    final List list = new ArrayList();
    final FeatureType ft = loadCSVIntoList( list, reader, comment, delemiter, lineskip );

    // featurelist erzeugen
    final Feature rootFeature = ShapeSerializer.createShapeRootFeature( ft );
    final List flist = (List)rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    flist.addAll( list );

    return new GMLWorkspace_Impl( new FeatureType[]
    {
        rootFeature.getFeatureType(),
        ft }, rootFeature, null, null, null, new HashMap() );
  }

  private FeatureType loadCSVIntoList( final List list, final Reader reader, final String comment,
      final String delemiter, final int lineskip ) throws IOException, CsvException
  {
    final FeatureTypeProperty[] props = (FeatureTypeProperty[])m_infos.keySet().toArray( new FeatureTypeProperty[0] );
    final FeatureType featureType = FeatureFactory.createFeatureType( "csv", null, props, null, null, null,
        new HashMap() );

    final LineNumberReader lnr = new LineNumberReader( reader );
    int skippedlines = 0;
    while( lnr.ready() )
    {
      final String line = lnr.readLine();
      if( line == null )
        break;
      if( skippedlines < lineskip )
      {
        skippedlines++;
        continue;
      }

      if( line.startsWith( comment ) )
        continue;

      final String[] tokens = line.split( delemiter );
      list.add( createFeatureFromTokens( "" + lnr.getLineNumber(), tokens, featureType ) );
    }
    return featureType;
  }

  private Feature createFeatureFromTokens( final String index, final String[] tokens, final FeatureType featureType )
      throws CsvException
  {
    final FeatureTypeProperty[] properties = featureType.getProperties();
    final Object[] data = new Object[properties.length];
    for( int i = 0; i < data.length; i++ )
    {
      final FeatureTypeProperty ftp = properties[i];
      final CSVInfo info = (CSVInfo)m_infos.get( ftp );

      // check column numbers
      for( int j = 0; j < info.columns.length; j++ )
      {
        final int colNumber = info.columns[j];
        if( colNumber >= tokens.length )
          throw new CsvException( "Zeile " + index + ": Spaltenindex " + colNumber + " zu gro� f�r FeatureProperty '"
              + ftp.getName() + "'" + "\nNur " + tokens.length + " Spalten gefunden." );
      }

      data[i] = parseColumns( ftp.getType(), info.format, info.columns, tokens, info.ignoreFormatExceptions );
    }

    return FeatureFactory.createFeature( index, featureType, data );
  }

  private static Object parseColumns( final String type, final String format, final int[] columns, final String[] tokens,
      final boolean ignoreFormatExceptions ) throws CsvException
  {
    try
    {
      final String[] input = new String[columns.length];
      for( int i = 0; i < input.length; i++ )
        input[i] = tokens[columns[i]];

      final Object data = FeatureUtils.createFeaturePropertyFromStrings( type, format, input );
      
      if( data == null )
        throw new CsvException( "Unbekannter Datentyp: " + type );
      
      return data;
    }
    catch( final NumberFormatException nfe )
    {
      if( ignoreFormatExceptions )
        return null;
      
      final String colStr = ArrayUtils.toString( columns );
      throw new CsvException( "Formatfehler beim Lesen der Spalten: " + colStr, nfe );
    }
  }
}