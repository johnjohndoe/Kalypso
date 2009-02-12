import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.FeatureLayer;
import org.deegree.model.feature.Feature;

/*
 * Created on 04.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author kraessig
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TeilgebietFactory
{
    public static Teilgebiet[] createTeilgebiete(FeatureLayer tgLayer,Block[] blocks)
    {
        List result=new ArrayList();
        Feature fe[]=tgLayer.getAllFeatures();
        for(int f=0;f<fe.length;f++)
        {
            Teilgebiet tg=new Teilgebiet(fe[f].getProperty("TEILGEBNR").toString());
            result.add(tg);
            //SK Test ob Block innerhalb TG
            //System.out.println(fe[f].getProperty("TG_NR").toString());
            for(int b=0;b<blocks.length;b++)
            {
                if(blockIsInside(blocks[b],fe[f]))
                {
                	//SK Testob Block innerhalb TG
                	//System.out.println(blocks[b].getPosition().getX()+" "+blocks[b].getPosition().getY());
                	
                	tg.add(blocks[b]);    
                }
            }
            
        }
        return (Teilgebiet[]) result.toArray(new Teilgebiet[result.size()]);
    }
    
    private static boolean blockIsInside(Block block,Feature fe)
    {
        return fe.getDefaultGeometryProperty().contains(block.getPosition());
        
        
    }
    
}
