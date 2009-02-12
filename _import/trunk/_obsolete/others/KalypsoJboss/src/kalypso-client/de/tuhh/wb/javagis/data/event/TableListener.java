package de.tuhh.wb.javagis.data.event;

public interface TableListener
{
    public void onTableElementCreate(int elementTable,Object eId);
    public void onTableElementRemove(int elementTable,Object eId);
    public void onSimplePropertyChanged(int elementTable,Object eId);
}
