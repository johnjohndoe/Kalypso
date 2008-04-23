package org.kalypso.ogc.gml.serialize;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * Lädt und schreibt ein CSV als {@link org.kalypsodeegree.model.feature.GMLWorkspace}. Die Information, welche Spalte
 * wie gelesen wird, wird per {@link #addInfo(IPropertyType, CSVInfo)}übergeben.
 * 
 * @todo Einerseits ganz schön, genau zu spezifizieren, was die Spalten sind. Alternativ wäre aber auch super, wenn das
 *       auch automatisch anhand der 1.Zeile ginge
 * @todo Koordinatensystem berücksichtigen
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
  private final Map<IPropertyType, CSVInfo> m_infos = new LinkedHashMap<IPropertyType, CSVInfo>();

  public final void addInfo( final IPropertyType ftp, final CSVInfo info )
  {
    m_infos.put( ftp, info );
  }

  public final GMLWorkspace loadCSV( final Reader reader, final String comment, final String delemiter, final int lineskip ) throws IOException, CsvException
  {
    final List<Feature> list = new ArrayList<Feature>();

    final IPropertyType[] props = m_infos.keySet().toArray( new IPropertyType[0] );
    final IFeatureType ft = GMLSchemaFactory.createFeatureType( new QName( "namespace", "csv" ), props );

    final Feature rootFeature = ShapeSerializer.createShapeRootFeature( ft );
    final IRelationType memberRelation = (IRelationType) rootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    loadCSVIntoList( rootFeature, memberRelation, ft, list, reader, comment, delemiter, lineskip );

    final GMLSchema schema = null;
    final URL context = null;
    final String schemaLocation = null;
    return new GMLWorkspace_Impl( schema, new IFeatureType[] { rootFeature.getFeatureType(), ft }, rootFeature, context, null, schemaLocation, null );
  }

  private void loadCSVIntoList( final Feature parent, final IRelationType parentRelation, final IFeatureType ft, final List<Feature> list, final Reader reader, final String comment, final String delemiter, final int lineskip ) throws IOException, CsvException
  {
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
      list.add( createFeatureFromTokens( parent, parentRelation, "" + lnr.getLineNumber(), tokens, ft ) );
    }
    return;
  }

  private Feature createFeatureFromTokens( final Feature parent, final IRelationType parentRelation, final String index, final String[] tokens, final IFeatureType featureType ) throws CsvException
  {
    final IPropertyType[] properties = featureType.getProperties();
    final Object[] data = new Object[properties.length];
    for( int i = 0; i < data.length; i++ )
    {
      final IPropertyType ftp = properties[i];
      if( !(ftp instanceof IValuePropertyType) )
        continue;
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final CSVInfo info = m_infos.get( ftp );

      // check column numbers
      for( final int colNumber : info.columns )
      {
        if( colNumber >= tokens.length )
          throw new CsvException( "Zeile " + index + ": Spaltenindex " + colNumber + " zu groß für FeatureProperty '" + ftp.getQName() + "'" + "\nNur " + tokens.length + " Spalten gefunden." );
      }

      data[i] = parseColumns( vpt, info.format, info.columns, tokens, info.ignoreFormatExceptions );
    }

    return FeatureFactory.createFeature( parent, parentRelation, index, featureType, data );
  }

  private static Object parseColumns( final IValuePropertyType vpt, final String format, final int[] columns, final String[] tokens, final boolean ignoreFormatExceptions ) throws CsvException
  {
    try
    {
      final String[] input = new String[columns.length];
      for( int i = 0; i < input.length; i++ )
        input[i] = tokens[columns[i]];

      final Object data = FeatureHelper.createFeaturePropertyFromStrings( vpt, format, input );

      if( data == null )
        throw new CsvException( "Unbekannter Datentyp: " + vpt.getQName() );

      return data;
    }
    catch( final NumberFormatException nfe )
    {
      if( ignoreFormatExceptions )
        return null;

      throw new CsvException( "Formatfehler beim Lesen der Spalten: " + columns, nfe );
    }
  }
}