package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Interface to be implemented by classes representing a wbr:RoughnessCollection elemenent
 * 
 * @author Patrice Congo
 */
public interface IRoughnessClsCollection extends IFeatureWrapperCollection<IRoughnessCls>
{
  /**
   * To get the name the roughness collection
   * 
   * @return the name of the roughness collection
   */
  public String getName( );

  /**
   * To set the name of the roughness collection
   * 
   * @param name
   *          the new name for the roughness collection
   * @throws IllegalArgumentException
   *           if name is null or an empty string
   */
  public void setName( String name ) throws IllegalArgumentException;

  /**
   * To get the roughness with the specified uri
   * 
   * @param uri
   *          the uri of the roughness to look-up
   * @return the roughness with the specified uri
   * @throws IllegalArgumentException
   *           if uri is null or an empty string
   */
  public IRoughnessCls getRoughnessByURI( String uri ) throws IllegalArgumentException;

  /**
   * Select all rougthness in the collection with a name matching the the given regular expression
   * 
   * @param nameRegExp --
   * @return
   */
  public List<IRoughnessCls> selectRoughnessByName( String nameRegExp );

}
