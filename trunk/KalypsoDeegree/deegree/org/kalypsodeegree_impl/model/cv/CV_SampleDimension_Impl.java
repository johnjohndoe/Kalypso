/*
 * OpenGIS� Grid Coverage Implementation Specification
 * 
 * This Java profile is derived from OpenGIS's specification available on their
 * public web site:
 * 
 * http://www.opengis.org/techno/implementation.htm
 * 
 * You can redistribute it, but should not modify it unless for greater OpenGIS
 * compliance.
 */
package org.deegree_impl.model.cv;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Iterator;

import org.opengis.cs.CS_Unit;
import org.opengis.cv.CV_ColorInterpretation;
import org.opengis.cv.CV_PaletteInterpretation;
import org.opengis.cv.CV_SampleDimension;
import org.opengis.cv.CV_SampleDimensionType;
import org.opengis.gc.GC_Parameter;

/**
 * Contains information for an individual sample dimension of coverage. This
 * interface is applicable to any coverage type. For grid coverages, the sample
 * dimension refers to an individual band.
 * 
 * @version 1.00
 * @since 1.00
 */
public class CV_SampleDimension_Impl implements CV_SampleDimension, Serializable
{
  private CV_ColorInterpretation colorInterpretation = null;

  private String description = null;

  private double maximumValue = 0;

  private double minimumValue = 0;

  private HashMap metadata = null;

  private double[] noData = null;

  private int offset = -1;

  private int[][] colorPalette = null;

  private CV_PaletteInterpretation paletteInterpretation = null;

  private CV_SampleDimensionType sampleDimensionType = null;

  private double scale = -1;

  private CS_Unit units = null;

  public CV_SampleDimension_Impl( String[] categoryNames, String description, double minimumValue,
      double maximumValue, GC_Parameter[] metadata, double[] noData, int offset,
      int[][] colorPalette, double scale, CS_Unit units,
      CV_ColorInterpretation colorInterpretation, CV_PaletteInterpretation paletteInterpretation,
      CV_SampleDimensionType sampleDimensionType )
  {
    this.maximumValue = maximumValue;
    this.minimumValue = minimumValue;
    this.scale = scale;
    this.noData = noData;
    this.offset = offset;
    this.colorPalette = colorPalette;
    this.description = description;
    this.metadata = new HashMap();
    if( metadata != null )
    {
      for( int i = 0; i < metadata.length; i++ )
      {
        this.metadata.put( metadata[i].getName(), metadata[i].getValue() );
      }
    }
    this.units = units;
    if( colorInterpretation == null )
    {
      this.colorInterpretation = new CV_ColorInterpretation( CV_ColorInterpretation.CV_Undefined );
    }
    else
    {
      this.colorInterpretation = colorInterpretation;
    }
    if( paletteInterpretation == null )
    {
      this.paletteInterpretation = new CV_PaletteInterpretation( CV_PaletteInterpretation.CV_Gray );
    }
    else
    {
      this.paletteInterpretation = paletteInterpretation;
    }
    this.sampleDimensionType = sampleDimensionType;

  }

  /**
   * Sequence of category names for the values contained in a sample dimension.
   * This allows for names to be assigned to numerical values. The first entry
   * in the sequence relates to a cell value of zero. For grid coverages,
   * category names are only valid for a classified grid data.
   * 
   * For example: <br>
   * <UL>
   * <li>0 Background</li>
   * <li>1 Water</li>
   * <li>2 Forest</li>
   * <li>3 Urban</li>
   * </UL>
   * Note: If no category names exist, an empty sequence is returned.
   * 
   * @return the sequence of category names for the values contained in a sample
   *         dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String[] getCategoryNames() throws RemoteException
  {
    return null;
  }

  /**
   * Color interpretation of the sample dimension. A sample dimension can be an
   * index into a color palette or be a color model component. If the sample
   * dimension is not assigned a color interpretation the value is
   * {@link CV_ColorInterpretation#CV_Undefined CV_Undefined}.
   * 
   * @return the color interpretation of the sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CV_ColorInterpretation getColorInterpretation() throws RemoteException
  {
    return colorInterpretation;
  }

  /**
   * Sample dimension title or description. This string may be null or empty if
   * no description is present.
   * 
   * @return the sample dimension title or description.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getDescription() throws RemoteException
  {
    return description;
  }

  /**
   * The maximum value occurring in the sample dimension. If this value is not
   * available, this value can be determined from the
   * {@link org.opengis.gp.GP_GridAnalysis#getMaxValue}operation. This value
   * can be empty if this value is not provided by the implementation.
   * 
   * @return the maximum value occurring in the sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double getMaximumValue() throws RemoteException
  {
    return maximumValue;
  }

  /**
   * The list of metadata keywords for a sample dimension. If no metadata is
   * available, the sequence will be empty.
   * 
   * @return the list of metadata keywords for a sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String[] getMetaDataNames() throws RemoteException
  {
    Iterator iterator = metadata.keySet().iterator();
    String[] names = new String[metadata.size()];
    int i = 0;
    while( iterator.hasNext() )
    {
      names[i++] = (String)iterator.next();
    }
    return names;
  }

  /**
   * Retrieve the metadata value for a given metadata name.
   * 
   * @param name
   *          Metadata keyword for which to retrieve metadata.
   * @return the metadata value for a given metadata name.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getMetadataValue( String name ) throws RemoteException
  {
    return (String)metadata.get( name );
  }

  /**
   * The minimum value occurring in the sample dimension. If this value is not
   * available, this value can be determined from the
   * {@link org.opengis.gp.GP_GridAnalysis#getMinValue}operation. This value
   * can be empty if this value is not provided by the implementation.
   * 
   * @return the minimum value occurring in the sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double getMinimumValue() throws RemoteException
  {
    return minimumValue;
  }

  /**
   * Values to indicate no data values for the sample dimension. For low
   * precision sample dimensions, this will often be no data values.
   * 
   * @return the values to indicate no data values for the sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double[] getNoDataValue() throws RemoteException
  {
    return noData;
  }

  /**
   * Offset is the value to add to grid values for this sample dimension. This
   * attribute is typically used when the sample dimension represents elevation
   * data. The default for this value is 0.
   * 
   * @return the offset is the value to add to grid values for this sample
   *         dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double getOffset() throws RemoteException
  {
    return offset;
  }

  /**
   * Color palette associated with the sample dimension. A color palette can
   * have any number of colors. See palette interpretation for meaning of the
   * palette entries. If the grid coverage has no color palette, an empty
   * sequence will be returned.
   * 
   * @return the color palette associated with the sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int[][] getPalette() throws RemoteException
  {
    return colorPalette;
  }

  /**
   * Indicates the type of color palette entry for sample dimensions which have
   * a palette. If a sample dimension has a palette, the color interpretation
   * must be {@link CV_ColorInterpretation#CV_GrayIndex CV_GrayIndex}or
   * {@link CV_ColorInterpretation#CV_PaletteIndex CV_PaletteIndex}. A palette
   * entry type can be Gray, RGB, CMYK or HLS.
   * 
   * @return the type of color palette entry for sample dimensions which have a
   *         palette.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CV_PaletteInterpretation getPaletteInterpretation() throws RemoteException
  {
    return paletteInterpretation;
  }

  /**
   * A code value indicating grid value data type. This will also indicate the
   * number of bits for the data type.
   * 
   * @return a code value indicating grid value data type.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CV_SampleDimensionType getSampleDimensionType() throws RemoteException
  {
    return sampleDimensionType;
  }

  /**
   * Scale is the value which is multiplied to grid values for this sample
   * dimension. This attribute is typically used when the sample dimension
   * represents elevation data. The default for this value is 1.
   * 
   * @return the scale.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double getScale() throws RemoteException
  {
    return scale;
  }

  /**
   * The unit information for this sample dimension. This interface typically is
   * provided with grid coverages which represent digital elevation data. This
   * value will be <code>null</code> if no unit information is available.
   * 
   * @return the unit information for this sample dimension.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CS_Unit getUnits() throws RemoteException
  {
    return units;
  }

}