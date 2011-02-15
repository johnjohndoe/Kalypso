package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.List;

import org.kalypso.afgui.model.IModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Interface to be implemented by classes representing a wbr:RoughnessCollection elemenent
 * 
 * @author Patrice Congo
 */
public interface IRoughnessClsCollection extends IFeatureWrapperCollection<IRoughnessCls>, IModel
{
  /**
   * To get the name the roughness collection
   * 
   * @return the name of the roughness collection
   */
  @Override
  public String getName( );

  /**
   * To set the name of the roughness collection
   * 
   * @param name
   *          the new name for the roughness collection
   * @throws IllegalArgumentException
   *           if name is null or an empty string
   */
  @Override
  public void setName( String name ) throws IllegalArgumentException;

  /**
   * Select all rougthness in the collection with a name matching the the given regular expression
   * 
   * @param nameRegExp
   *          --
   * @return
   */
  public List<IRoughnessCls> selectRoughnessByName( String nameRegExp );

}
