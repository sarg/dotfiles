-- Moosic bindings for Lua
-- Author Sergey Trofimov 
-- sarg@icn.bmstu.ru
-- 

local moosic = '/home/sarg/devel/bin/moosic '

function playthis(cmd)
	local f = io.popen(moosic..cmd,'r');
	if f then
		f:close();
	end
end

function playlist_menu()
	local entrys = {};
	local n=0;
	local en="";

	local f = io.popen(moosic..'curr', 'r');
	if f then
		en = f:read();
		if en~="" then
			table.insert(entrys, {'<span foreground="yellow">&gt; '..en..'</span>', ""});
			n=n+1;
		end 
		f:close();
	end

	f = assert(io.popen(moosic..'plainlist', 'r'));
	for en in f:lines() do
        if en~="" then
            if n~=0 then
                table.insert(entrys, {en, moosic.."next "..n}); 
            else
                table.insert(entrys, {'<span foreground="yellow">'..en..'</span>', moosic..'play'}); 
            end 
            n=n+1;
        end
	end
	f:close(); 

	return entrys;
end
