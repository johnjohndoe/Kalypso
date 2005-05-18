package org.kalypso.ogc.gml.serialize;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Lädt und schreibt ein CSV als
 * {@link org.kalypsodeegree.model.feature.GMLWorkspace}.
 * 
 * Die Information, welche Spalte wie gelesen wird, wird per
 * {@link #addInfo(FeatureTypeProperty, CSVInfo)}übergeben.
 * 
 * @todo Einerseits ganz schön, genau zu spezifizieren, was die Spalten sind.
 *       Alternativ wäre aber auch super, wenn das auch automatisch anhand der
 *       1.Zeile ginge
 * 
 * @todo Koordinatensystem berücksichtigen
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

    public CSVInfo( final String format, final int[] columns, final boolean ignoreFormatExceptions )
    {
      this.format = format;
      this.columns = columns;
      this.ignoreFormatExceptions = ignoreFormatExceptions;
    }
  }

  /** featureTypeProperty -> cvsinfo */
  private Map m_infos = new LinkedHashMap();

  public final void addInfo( final FeatureTypeProperty ftp, final CSVInfo info )
  {
    m_infos.put( ftp, info );
  }

  public final GMLWorkspace loadCSV( final Reader reader, final String comment,
      final String delemiter, final int lineskip ) throws IOException, CsvException
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
    final FeatureTypeProperty[] props = (FeatureTypeProperty[])m_infos.keySet().toArray(
        new FeatureTypeProperty[0] );
    final FeatureType featureType = FeatureFactory.createFeatureType( "csv", null, props, null,
        null, null, new HashMap() );

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

  private Feature createFeatureFromTokens( final String index, final String[] tokens,
      final FeatureType featureType ) throws CsvException
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
          throw new CsvException( "Zeile " + index + ": Spaltenindex " + colNumber
              + " zu groß für FeatureProperty '" + ftp.getName() + "'" + "\nNur " + tokens.length
              + " Spalten gefunden." );
      }

      data[i] = parseColumns( ftp.getType(), info.format, info.columns, tokens, info.ignoreFormatExceptions );
    }

    return FeatureFactory.createFeature( index, featureType, data );
  }
  
  private Object parseColumns( final String type, final String format, final int[] columns, final String[] tokens, final boolean ignoreFormatExceptions ) throws CsvException
  {
    try
    {
      final int col0 = columns[0];
      if( String.class.getName().equals( type ) )
        return tokens[col0];

      if( Integer.class.getName().equals( type ) )
        return new Integer( tokens[col0] );

      if( Long.class.getName().equals( type ) )
        return new Long( tokens[col0] );

      if( Float.class.getName().equals( type ) )
        return new Float( tokens[col0] );

      if( Double.class.getName().equals( type ) )
        return new Double( tokens[col0].replace( ',', '.' ) );

      if( GM_Point.class.getName().equals( type ) )
      {
        final int col1 = columns[1];
        final String rwString = tokens[col0].trim();
        final String hwString = tokens[col1].trim();
        if( rwString == null || rwString.length() == 0 || hwString == null
            || hwString.length() == 0 )
          return null;

        final double rw = Double.parseDouble( rwString );
        final double hw = Double.parseDouble( hwString );

        final CS_CoordinateSystem crs = ConvenienceCSFactory.getInstance().getOGCCSByName( format );

        return GeometryFactory.createGM_Point( rw, hw, crs );
      }
    }
    catch( final NumberFormatException nfe )
    {
      if( ignoreFormatExceptions )
        return null;
      
      throw new CsvException( "Formatfehler beim Lesen der Spalten: " + columns, nfe );
    }
    
    throw new CsvException( "Unbekannter Datentyp: " + type );
  }
}